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

-- | This is the function that actually makes sure we run the parser on content. Everything that
-- follows from the invocation of `hostsFileP` is going to be executing in our `Parser` context.
parseHostsFile :: Filename -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) [HostEntry]
parseHostsFile (Filename filename) =
  -- The reason we use `runParser` here instead of `runParserT` is because we have the `Identity`
  -- monad as our baked in monad. This way we will automatically unwrap the value since it doesn't
  -- depend on any interesting `m` type.
  Megaparsec.runParser hostsFileP filename

hostsFileP :: Parser [HostEntry]
hostsFileP =
  -- `sepBy1` takes two parsers, one for each thing we want to parse and one for what separates them.
  -- In this case we know that we want to parse several `HostEntry`s and that they are separated by
  -- newlines.
  Megaparsec.sepBy1 hostEntryP MChar.newline

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
  d <- Lexer.decimal

  pure $ IPAddress a b c d

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
