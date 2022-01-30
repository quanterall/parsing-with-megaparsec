module ScriptingLanguage where

import HostsParser (Filename (..))
import RIO
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Megaparsec.ParsecT Void Text (RIO ParsingState)

data ParsingState = ParsingState
  { bindingsRef :: !(IORef (Map Text Expression))
  }

data ScriptComponent
  = Statement !Statement
  | Expression !Expression
  deriving (Eq, Show)

data Statement
  = AssignValue !Text !Expression
  | IfStatement !Expression ![ScriptComponent] ![ScriptComponent]
  deriving (Eq, Show)

data Expression
  = StringLiteral !Text
  | IntegerLiteral !Integer
  | FloatLiteral !Double
  | BooleanLiteral !Bool
  | ShellCommand !(Maybe ShellCommandComponent)
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
scriptP = undefined

-- | Defines how whitespace is consumed.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space MChar.space1 (Lexer.skipLineComment "# ") Megaparsec.empty

-- | Reads any amount of whitespace and then applies a parser.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Reads any amount of whitespace and then a specific string of text.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer
