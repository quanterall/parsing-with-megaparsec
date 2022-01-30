module HostsParserSpec where

import HostsParser
import RIO
import qualified RIO.Text as Text
import Test.Hspec
import Text.Megaparsec.Error

spec :: Spec
spec = do
  describe "`parseHostsText`" $ do
    it "parses a single host" $ do
      let text = "127.0.0.1 localhost"
          parseResult = parseHostsFile (Filename "/etc/hosts") text
      expectRight parseResult
      parseResult
        `shouldBe` Right
          [ HostEntry {hostEntryIp = IPAddress 127 0 0 1, hostEntryHostnames = ["localhost"]}
          ]

    it "parses any amount of spaces between IP address and host names, as well as before IP" $ do
      let text = "   127.0.0.1    localhost"
          parseResult = parseHostsFile (Filename "/etc/hosts") text
      expectRight parseResult
      parseResult
        `shouldBe` Right
          [ HostEntry {hostEntryIp = IPAddress 127 0 0 1, hostEntryHostnames = ["localhost"]}
          ]

    it "Parses multiple hosts" $ do
      let text = "  127.0.0.1  localhost\n192.168.0.2  omniknight\t omniknight.local"
          parseResult = parseHostsFile (Filename "/etc/hosts") text
      expectRight parseResult
      parseResult
        `shouldBe` Right
          [ HostEntry {hostEntryIp = IPAddress 127 0 0 1, hostEntryHostnames = ["localhost"]},
            HostEntry
              { hostEntryIp = IPAddress 192 168 0 2,
                hostEntryHostnames =
                  [ "omniknight",
                    "omniknight.local"
                  ]
              }
          ]

expectRight :: Either (ParseErrorBundle Text Void) a -> Expectation
expectRight (Right _) = return ()
expectRight (Left e) = expectationFailure $ errorBundlePretty e
