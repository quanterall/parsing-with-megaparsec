module HostsParser where

import RIO
import qualified RIO.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Megaparsec.ParsecT Void Text Identity

newtype Filename = Filename {unFilename :: String}
  deriving (Eq, Show)

data HostEntry = HostEntry
  { hostEntryIp :: !IPAddress,
    hostEntryHostnames :: ![Text]
  }
  deriving (Eq, Show)

data IPAddress = IPAddress !Int !Int !Int !Int
  deriving (Eq, Show)

-- | This is the function we use in order to actually read a file and parse it.
parseHostsFile :: FilePath -> IO (Either (Megaparsec.ParseErrorBundle Text Void) [HostEntry])
parseHostsFile path = do
  contents <- readFileUtf8 path
  pure $ parseHostsText (Filename path) contents

-- | This is the function that actually makes sure we run the parser on content. Everything that
-- follows from the invocation of `hostsFileP` is going to be executing in our `Parser` context.
parseHostsText :: Filename -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) [HostEntry]
parseHostsText (Filename filename) =
  -- The reason we use `runParser` here instead of `runParserT` is because we have the `Identity`
  -- monad as our baked in monad. This way we will automatically unwrap the value since it doesn't
  -- depend on any interesting `m` type.
  --
  -- Note how we consume all initial whitespace in the file before we start parsing. This turns out
  -- to be a fairly common occurrence, as it's common for configuration files and other types of
  -- files to start with comments in the language/format that they're written in.
  Megaparsec.runParser (spaceConsumer *> lexeme hostsFileP) filename

-- | This is a parser for what we can consider the entire file, so it's in effect a parser that
-- should, in a higher-level manner, describe the entire file.
hostsFileP :: Parser [HostEntry]
hostsFileP =
  -- Each entry is parsed with `lexeme hostentryP`, which means that we'll consume all whitespace
  -- after the entry. This means we can just say "We want to read many host entries and whitespace
  -- after", as each entry will follow the whitespace that is also consumed.
  many $ lexeme hostEntryP

hostEntryP :: Parser HostEntry
hostEntryP = do
  -- The line can start with any amount of space characters and then we'll expect to read an IP
  -- address. Note how we are using `*>` here to say that the thing on the right is what we want the
  -- expression to return, but we still want to run the thing on the left. The result of the left
  -- expression is going to be discarded.
  ipAddress <- MChar.space *> ipAddressP

  -- We expect at least a single space between the IP address and the host names, but we will
  -- consume more if available. We use `hspace1` because we specifically expect spaces, not other
  -- whitespace like newlines.
  MChar.hspace1

  -- After we've consumed all the spaces we want to read a list of host names separated by some
  -- amount of spaces. We use `hspace1` because it does not consume newlines.
  hostnames <- Megaparsec.sepBy1 hostNameP MChar.hspace1

  -- If we've reached this point, none of the parsers have failed
  pure HostEntry {hostEntryIp = ipAddress, hostEntryHostnames = hostnames}

ipAddressP :: Parser IPAddress
ipAddressP = do
  -- We want to parse exactly four integers separated by periods. If we were interested in parsing
  -- any amount of digits separated by periods we could use `sepBy1 Lexer.decimal (MChar.char '.')`

  -- Note how we use the `<*` here to say that whatever is on the left side is what we want the
  -- expression to actually return, but we still want to execute the thing on the right.
  a <- Lexer.decimal <* MChar.char '.'
  b <- Lexer.decimal <* MChar.char '.'
  c <- Lexer.decimal <* MChar.char '.'
  IPAddress a b c <$> Lexer.decimal

hostNameP :: Parser Text
hostNameP = do
  -- We want our host names to start with letters, so let's say that explicitly.
  firstCharacter <- MChar.letterChar
  -- The remaining characters can really be any of several choices:
  -- letters, digits, underscores, dashes or periods.
  remainingCharacters <- some hostNameCharacterP

  pure $ Text.pack (firstCharacter : remainingCharacters)

hostNameCharacterP :: Parser Char
hostNameCharacterP =
  Megaparsec.choice
    [ MChar.letterChar,
      MChar.digitChar,
      MChar.char '_',
      MChar.char '-',
      MChar.char '.'
    ]

-- | Defines how whitespace is consumed.
spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    skipSpaces
    (Lexer.skipLineComment "#")
    Megaparsec.empty
  where
    skipSpaces = MChar.space1 <|> Megaparsec.skipSome (MChar.char '\xfeff')

-- | Applies a parser and any amount of whitespace after.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Reads a specific string of text and any amount of whitespace after.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer
