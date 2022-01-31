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

data Expression
  = StringLiteral !Text
  | IntegerLiteral !Integer
  | FloatLiteral !Double
  | BooleanLiteral !Bool
  | InterpolatedString ![StringInterpolationFragment]
  | ShellCommand ![ShellCommandText] !(Maybe ShellCommandComponent)
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
  runRIO initialState $ Megaparsec.runParserT scriptP filename text

scriptP :: Parser [ScriptComponent]
scriptP =
  Megaparsec.sepBy
    (Megaparsec.choice [Statement <$> statementP, Expression <$> expressionP])
    (some MChar.newline)

statementP :: Parser Statement
statementP = do
  Megaparsec.choice [assignValueP, ifStatementP]

assignValueP :: Parser Statement
assignValueP = do
  bindingName <- lexeme bindingNameP
  _ <- symbol "="
  expression <- expressionP
  bindValue bindingName expression
  pure $ AssignValue bindingName expression

bindValue :: BindingName -> Expression -> Parser ()
bindValue bindingName expression = do
  ref <- asks bindingsRef
  modifyIORef ref $ Map.insert bindingName expression

bindingNameP :: Parser BindingName
bindingNameP = do
  initialCharacter <- MChar.letterChar
  restOfName <- Megaparsec.many (MChar.alphaNumChar <|> MChar.char '_')
  pure $ BindingName $ Text.pack (initialCharacter : restOfName)

ifStatementP :: Parser Statement
ifStatementP = undefined

expressionP :: Parser Expression
expressionP =
  Megaparsec.choice
    [ stringLiteralP,
      integerLiteralP,
      floatLiteralP,
      booleanLiteralP,
      interpolatedStringP,
      shellCommandP
    ]

stringLiteralP :: Parser Expression
stringLiteralP = do
  _ <- MChar.char '\"'
  string <-
    Megaparsec.manyTill (Megaparsec.try readEscapedQuote <|> MChar.printChar) (MChar.char '\"')
  pure $ StringLiteral $ Text.pack string
  where
    readEscapedQuote :: Parser Char
    readEscapedQuote = do
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
    shellLiteralCharacterP = Megaparsec.satisfy (/= '\'')

-- | Defines how whitespace is consumed.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space MChar.space1 (Lexer.skipLineComment "// ") Megaparsec.empty

-- | Applies a parser and any amount of whitespace after.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Reads a specific string of text and any amount of whitespace after.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer
