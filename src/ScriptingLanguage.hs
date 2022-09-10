module ScriptingLanguage where

import HostsParser (Filename (..))
import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Megaparsec.ParsecT Void Text (RIO ParsingState)

newtype BindingName = BindingName {unBindingName :: Text}
  deriving (Eq, Ord, Show)

data ParsingState = ParsingState
  { bindingsRef :: !(IORef (Map BindingName Expression))
  }

data ScriptComponent
  = Statement !Statement
  | Expression !Expression
  deriving (Eq, Show)

data Statement
  = AssignValue !BindingName !Expression
  | IfStatement !Expression ![ScriptComponent] ![ScriptComponent]
  deriving (Eq, Show)

-- | An expression is a piece of code that can be evaluated and has a return value after evaluation.
data Expression
  = -- | "A string"
    StringLiteral !Text
  | -- | 42
    IntegerLiteral !Integer
  | -- | 1337.0
    FloatLiteral !Double
  | -- | True / False
    BooleanLiteral !Bool
  | -- | `A string with a {binding}`
    InterpolatedString ![StringInterpolationFragment]
  | -- | 'ls -l `{inputDirectory}`'
    -- 'ls -l `{inputDirectory}`'.out
    -- 'ls -l `{inputDirectory}`'.err
    -- 'ls -l `{inputDirectory}`'.code
    ShellCommand ![ShellCommandText] !(Maybe ShellCommandComponent)
  | -- | (When a binding with the name `binding` exists)
    -- binding
    BindingExpression !BindingName
  deriving (Eq, Show)

data ShellCommandText
  = ShellCommandLiteral !Text
  | ShellCommandInterpolation ![StringInterpolationFragment]
  deriving (Eq, Show)

data StringInterpolationFragment
  = LiteralFragment !Text
  | BindingFragment !BindingName
  deriving (Eq, Show)

data ShellCommandComponent
  = ShellStandardOut
  | ShellStandardError
  | ShellExitCode
  deriving (Eq, Ord, Show)

parseScript ::
  Filename ->
  Text ->
  IO (Either (Megaparsec.ParseErrorBundle Text Void) [ScriptComponent])
parseScript (Filename filename) text = do
  bindingsRef' <- newIORef mempty
  let initialState = ParsingState {bindingsRef = bindingsRef'}
  -- `runParserT` here is going to return a `RIO ParsingState (Either ...)` so we take that and
  -- give it to `runRIO` which will unpack that into an `IO (Either ...)`
  runRIO initialState $ Megaparsec.runParserT scriptComponentsP filename text

scriptComponentsP :: Parser [ScriptComponent]
scriptComponentsP =
  Megaparsec.sepEndBy
    (maybeWhiteSpaceAnd $ Megaparsec.choice [Statement <$> statementP, Expression <$> expressionP])
    (some MChar.newline)
  where
    maybeWhiteSpaceAnd :: Parser a -> Parser a
    maybeWhiteSpaceAnd p = optional spaceConsumer *> p

statementP :: Parser Statement
statementP = do
  Megaparsec.choice [ifStatementP, assignValueP]

assignValueP :: Parser Statement
assignValueP = do
  -- If we can read a binding name followed by an equals sign and an expressions we want to add it
  -- to the bindings map.
  bindingName <- lexeme bindingNameP
  _ <- symbol "="
  expression <- expressionP
  bindValue bindingName expression
  pure $ AssignValue bindingName expression

bindValue :: BindingName -> Expression -> Parser ()
bindValue bindingName expression = do
  -- We read our bindings reference (a map of binding names to expressions)
  ref <- asks bindingsRef
  -- We then modify it by inserting our expression in the corresponding slot
  modifyIORef ref $ Map.insert bindingName expression

bindingNameP :: Parser BindingName
bindingNameP = do
  -- A binding name starts with a lowercase letter
  initialCharacter <- MChar.lowerChar
  -- ... the rest of the characters can be any alphanumeric character or underscore
  restOfName <- Megaparsec.many (MChar.alphaNumChar <|> MChar.char '_')
  pure $ BindingName $ Text.pack (initialCharacter : restOfName)

availableBindingP :: Parser BindingName
availableBindingP = do
  -- An available binding is one where we can read a binding name and look it up in our bindings to
  -- verify that it is available.
  bindingName <- bindingNameP
  ref <- asks bindingsRef
  bindingExists <- liftIO $ Map.member bindingName <$> readIORef ref
  if bindingExists
    then pure bindingName
    else reportError $ "Binding " <> Text.unpack (unBindingName bindingName) <> " is not defined"

ifStatementP :: Parser Statement
ifStatementP = do
  -- An `if` statement starts with the literal symbol "if", then an expression to evalue for
  -- truthiness, then a list of statements to execute if the expression is true, and then the symbol
  -- "else" and a list of statements to execute if the expression is false.
  _ <- symbol "if "
  condition <- lexeme expressionP
  _ <- symbol "{"
  thenBranch <- lexeme scriptComponentsP
  _ <- symbol "}"
  _ <- symbol "else"
  _ <- symbol "{"
  elseBranch <- lexeme scriptComponentsP
  _ <- MChar.char '}'
  pure $ IfStatement condition thenBranch elseBranch

expressionP :: Parser Expression
expressionP =
  Megaparsec.choice
    [ -- The three different string openers are very distinct and can be matched very easily with an
      -- opening character (double quote, single quote or backtick), so we can put them first
      -- without worrying that anything else contends for the same opening input. This allows the
      -- parser to try to read the initial part, fail and move on to other alternatives with no
      -- consumed input.
      -- "..."
      stringLiteralP,
      -- `...`
      interpolatedStringP,
      -- '...'
      shellCommandP,
      booleanLiteralP,
      Megaparsec.try (BindingExpression <$> availableBindingP),
      -- The reason we want to use `floatLiteralP` here before `integerLiteralP` is that they start
      -- with the same thing; a (potentially) signed number. Float then requires more (a period).
      -- If we successfully read the number part, we don't want to fail because we failed to read
      -- the period, so we put the input back in the stream with `try`, allowing `integerLiteralP`
      -- to succeed.
      Megaparsec.try floatLiteralP,
      integerLiteralP
    ]

stringLiteralP :: Parser Expression
stringLiteralP = do
  -- A string starts with a double quote
  _ <- MChar.char '\"'
  -- The contents of it will be many printable characters, but we can also have escaped quotes,
  -- which we need to handle. The `try` function here will try to apply a parser, but if it fails
  -- will not end up consuming any input. This makes it so that we can say "Try to parse this but
  -- put the content back if you end up failing".
  -- When this first part fails, we'll move on tho `MChar.printChar` because we use `<|>`, the
  -- alternative operator.
  -- The string stops when we find a normal double quote.
  string <-
    Megaparsec.manyTill (Megaparsec.try readEscapedQuote <|> MChar.printChar) (MChar.char '\"')
  pure $ StringLiteral $ Text.pack string
  where
    readEscapedQuote :: Parser Char
    readEscapedQuote = do
      -- An escaped double quote is a backslash followed by a double quote
      MChar.char '\\' *> MChar.char '\"'

interpolatedStringP :: Parser Expression
interpolatedStringP = InterpolatedString <$> stringInterpolationFragmentsP

stringInterpolationFragmentsP :: Parser [StringInterpolationFragment]
stringInterpolationFragmentsP = do
  -- An interpolated string starts with a backtick and we read interpolation fragments until another
  -- backtick is read.
  MChar.char '`' *> Megaparsec.manyTill interpolationFragmentP (MChar.char '`')

interpolationFragmentP :: Parser StringInterpolationFragment
interpolationFragmentP =
  -- An interpolation fragment is either a binding fragment or a literal fragment. If reading a
  -- binding fragment fails, we'll put whatever we read back in the input stream and read it as a
  -- literal.
  Megaparsec.choice [Megaparsec.try bindingFragmentP, literalFragmentP]

bindingFragmentP :: Parser StringInterpolationFragment
bindingFragmentP = do
  -- A binding fragment is a curly brace followed by an available binding name, then a closing curly
  -- brace.
  _ <- MChar.char '{'
  bindingName <- availableBindingP
  _ <- MChar.char '}'
  pure $ BindingFragment bindingName

literalFragmentP :: Parser StringInterpolationFragment
literalFragmentP =
  -- A literal fragment is just all characters that aren't backticks or opening curly braces.
  (Text.pack >>> LiteralFragment) <$> Megaparsec.some literalFragmentCharacterP
  where
    literalFragmentCharacterP :: Parser Char
    literalFragmentCharacterP = Megaparsec.satisfy (`notElem` ['`', '{'])

integerLiteralP :: Parser Expression
integerLiteralP =
  -- We use `signed` here to say that we want the capability to read both negative and positive
  -- integer literals. The `pure ()` is how to read space between the sign and the number. Here we
  -- are saying that we don't want to consume any space.
  IntegerLiteral <$> Lexer.signed (pure ()) Lexer.decimal

floatLiteralP :: Parser Expression
floatLiteralP = FloatLiteral <$> Lexer.signed (pure ()) Lexer.float

booleanLiteralP :: Parser Expression
booleanLiteralP = do
  -- A boolean literal is either `True` or `False`. We read it by saying that there are two strings
  -- you are allowed to match, then we create the boolean by comparing the result to the literal
  -- string "True".
  text <- ["True", "False"] & fmap MChar.string & Megaparsec.choice
  pure $ BooleanLiteral $ text == "True"

shellCommandP :: Parser Expression
shellCommandP = do
  -- A shell command starts with a single quote followed by a special string that can either be
  -- shell command text or interpolated string text. It can then be followed by an accessor to say
  -- which part of the result we want to access.
  _ <- MChar.char '\''
  shellCommandText <- shellCommandTextP
  maybeShellCommandComponent <- Megaparsec.optional shellCommandComponentP
  pure $ ShellCommand shellCommandText maybeShellCommandComponent
  where
    shellCommandComponentP :: Parser ShellCommandComponent
    shellCommandComponentP = do
      -- A shell command component is an accessor for the result of a shell command. We might want
      -- to access standard out, standard error or the exit code.
      _ <- MChar.char '.'
      Megaparsec.choice
        [ MChar.string "out" $> ShellStandardOut,
          MChar.string "err" $> ShellStandardError,
          MChar.string "code" $> ShellExitCode
        ]

    shellCommandTextP :: Parser [ShellCommandText]
    shellCommandTextP =
      -- Shell command text is comprised of either string interpolation fragments or shell command
      -- literal text. This continues until we read a single quote.
      Megaparsec.manyTill
        ( Megaparsec.choice
            [ ShellCommandInterpolation <$> stringInterpolationFragmentsP,
              shellCommandLiteralP
            ]
        )
        (MChar.char '\'')

    shellCommandLiteralP :: Parser ShellCommandText
    shellCommandLiteralP =
      -- A shell command literal is just any character that is not a backtick or single quote.
      (Text.pack >>> ShellCommandLiteral) <$> Megaparsec.some shellLiteralCharacterP

    shellLiteralCharacterP :: Parser Char
    shellLiteralCharacterP = Megaparsec.satisfy (`notElem` ['\'', '`'])

-- | Defines how whitespace is consumed.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space MChar.space1 (Lexer.skipLineComment "//") Megaparsec.empty

-- | Applies a parser and any amount of whitespace after.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Reads a specific string of text and any amount of whitespace after.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

-- | Returns an error to be displayed.
reportError :: String -> Parser a
reportError = Megaparsec.ErrorFail >>> Set.singleton >>> Megaparsec.fancyFailure
