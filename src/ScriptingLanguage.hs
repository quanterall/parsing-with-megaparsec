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
  | -- | true / false
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
  -- A binding name starts with a letter
  initialCharacter <- MChar.letterChar
  -- ... the rest of the characters can be any alphanumeric character or underscore
  restOfName <- Megaparsec.many (MChar.alphaNumChar <|> MChar.char '_')
  pure $ BindingName $ Text.pack (initialCharacter : restOfName)

ifStatementP :: Parser Statement
ifStatementP = do
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
    [ stringLiteralP,
      integerLiteralP,
      floatLiteralP,
      booleanLiteralP,
      interpolatedStringP,
      shellCommandP,
      BindingExpression <$> availableBindingP
    ]

availableBindingP :: Parser BindingName
availableBindingP = do
  bindingName <- bindingNameP
  ref <- asks bindingsRef
  bindingExists <- liftIO $ Map.member bindingName <$> readIORef ref
  if bindingExists
    then pure bindingName
    else reportError $ "Binding " <> Text.unpack (unBindingName bindingName) <> " is not defined"

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
interpolatedStringP = do
  fragments <- stringInterpolationFragmentsP
  pure $ InterpolatedString fragments

stringInterpolationFragmentsP :: Parser [StringInterpolationFragment]
stringInterpolationFragmentsP = do
  _ <- MChar.char '`'
  Megaparsec.manyTill interpolationFragmentP (MChar.char '`')

interpolationFragmentP :: Parser StringInterpolationFragment
interpolationFragmentP =
  Megaparsec.choice [Megaparsec.try bindingFragmentP, literalFragmentP]

bindingFragmentP :: Parser StringInterpolationFragment
bindingFragmentP = do
  _ <- MChar.char '{'
  bindingName <- bindingNameP
  _ <- MChar.char '}'
  checkBoundVariable bindingName
  pure $ BindingFragment bindingName

checkBoundVariable :: BindingName -> Parser ()
checkBoundVariable bindingName = do
  reference <- asks bindingsRef
  bindings <- readIORef reference
  case Map.lookup bindingName bindings of
    Nothing -> reportError $ "Variable " <> show bindingName <> " is unbound"
    Just _ -> pure ()

reportError :: String -> Parser a
reportError = Megaparsec.ErrorFail >>> Set.singleton >>> Megaparsec.fancyFailure

literalFragmentP :: Parser StringInterpolationFragment
literalFragmentP = (Text.pack >>> LiteralFragment) <$> Megaparsec.some literalFragmentCharacterP
  where
    literalFragmentCharacterP :: Parser Char
    literalFragmentCharacterP = Megaparsec.satisfy (`notElem` ['`', '{'])

integerLiteralP :: Parser Expression
integerLiteralP = IntegerLiteral <$> Lexer.signed (pure ()) Lexer.decimal

floatLiteralP :: Parser Expression
floatLiteralP = FloatLiteral <$> Lexer.signed (pure ()) Lexer.float

booleanLiteralP :: Parser Expression
booleanLiteralP = do
  text <- ["true", "false"] & fmap MChar.string & Megaparsec.choice
  pure $ BooleanLiteral $ text == "true"

shellCommandP :: Parser Expression
shellCommandP = do
  _ <- MChar.char '\''
  shellCommandText <- shellCommandTextP
  maybeShellCommandComponent <- Megaparsec.optional shellCommandComponentP
  pure $ ShellCommand shellCommandText maybeShellCommandComponent
  where
    shellCommandComponentP :: Parser ShellCommandComponent
    shellCommandComponentP = do
      _ <- MChar.char '.'
      Megaparsec.choice
        [ MChar.string "out" *> pure ShellStandardOut,
          MChar.string "err" *> pure ShellStandardError,
          MChar.string "code" *> pure ShellExitCode
        ]

    shellCommandTextP :: Parser [ShellCommandText]
    shellCommandTextP =
      Megaparsec.manyTill
        ( Megaparsec.choice
            [ ShellCommandInterpolation <$> stringInterpolationFragmentsP,
              shellCommandLiteralP
            ]
        )
        (MChar.char '\'')

    shellCommandLiteralP :: Parser ShellCommandText
    shellCommandLiteralP =
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
