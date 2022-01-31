module ScriptingLanguageSpec where

import HostsParser (Filename (..))
import RIO
import ScriptingLanguage
import Test.Hspec
import Text.Megaparsec.Error

testFile :: FilePath
testFile = "test-data/test.glue"

spec :: Spec
spec = do
  describe "Parsing" $ do
    describe "Basic string assignment" $ do
      it "should create an instruction to store a string into a binding" $ do
        let text = "value = \"test\""
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (StringLiteral "test")]

      it "should handle escaped quotes in string values" $ do
        let text = "value = \"test\\\" value\""
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (StringLiteral "test\" value")]

      it "should be able to read two assignments in a row" $ do
        let text = "value1 = \"test1\"\nvalue2 = \"test2\""
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [ Statement $ AssignValue (BindingName "value1") (StringLiteral "test1"),
              Statement $ AssignValue (BindingName "value2") (StringLiteral "test2")
            ]

    describe "String interpolation" $ do
      it "should be able to interpolate a string without any binding fragments" $ do
        let text = "value = `test`"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "value")
                  (InterpolatedString [LiteralFragment "test"])
            ]

      it "should be able to interpolate a string with a binding fragment" $ do
        let text = "test = \"hello\"\nvalue = `test {test}`"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "test")
                  (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "value")
                  ( InterpolatedString
                      [ LiteralFragment "test ",
                        BindingFragment (BindingName "test")
                      ]
                  )
            ]

      it "should be able to interpolate a string both before and after a binding fragment" $ do
        let text = "test = \"hello\"\nvalue = `test {test} after as well`"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "test")
                  (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "value")
                  ( InterpolatedString
                      [ LiteralFragment "test ",
                        BindingFragment (BindingName "test"),
                        LiteralFragment " after as well"
                      ]
                  )
            ]

      it "should be able to interpolate a string, binding, string, binding and string" $ do
        let text = "test = \"hello\"\nvalue = `test {test} after {test} as well`"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "test")
                  (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "value")
                  ( InterpolatedString
                      [ LiteralFragment "test ",
                        BindingFragment (BindingName "test"),
                        LiteralFragment " after ",
                        BindingFragment (BindingName "test"),
                        LiteralFragment " as well"
                      ]
                  )
            ]

    describe "Shell commands" $ do
      it "should be able to assign a basic shell command result without any special fields" $ do
        let text = "value = 'echo test'"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "value")
                  (ShellCommand [ShellCommandLiteral "echo test"] Nothing)
            ]

      it "should be able to assign a basic shell command result and access stdout" $ do
        let text = "value = 'echo test'.out"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "value")
                  (ShellCommand [ShellCommandLiteral "echo test"] (Just ShellStandardOut))
            ]

      it "should be able to assign a basic shell command result and access stderr" $ do
        let text = "value = 'echo test'.err"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "value")
                  (ShellCommand [ShellCommandLiteral "echo test"] (Just ShellStandardError))
            ]

      it "should be able to assign a basic shell command result and access exit code" $ do
        let text = "value = 'echo test'.code"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "value")
                  (ShellCommand [ShellCommandLiteral "echo test"] (Just ShellExitCode))
            ]

expectRight :: Either (ParseErrorBundle Text Void) a -> Expectation
expectRight (Right _) = return ()
expectRight (Left e) = expectationFailure $ errorBundlePretty e
