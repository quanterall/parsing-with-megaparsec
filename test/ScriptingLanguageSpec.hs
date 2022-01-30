module ScriptingLanguageSpec where

import HostsParser (Filename (..))
import RIO
import qualified RIO.Text as Text
import ScriptingLanguage
import Test.Hspec

testFile :: FilePath
testFile = "test-data/test.glue"

spec :: Spec
spec = do
  describe "Parsing" $ do
    describe "Basic string assignment" $
      it "should create an instruction to store a string into a binding" $ do
        let text = "value = \"test\""
            result = parseScript (Filename "test.glue") text
        result `shouldReturn` Right [Statement $ AssignValue "value" (StringLiteral "test")]
