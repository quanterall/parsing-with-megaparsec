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
    describe "Basic assignment" $ do
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

      it "should be able to assign a integer" $ do
        let text = "value = 1"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (IntegerLiteral 1)]

      it "should be able to assign a negative integer" $ do
        let text = "value = -1"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (IntegerLiteral (-1))]

      it "should be able to assign a float" $ do
        let text = "value = 1.1"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (FloatLiteral 1.1)]

      it "should be able to assign a negative float" $ do
        let text = "value = -1.1"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (FloatLiteral (-1.1))]

      it "should be able to assign a `True`" $ do
        let text = "value = True"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (BooleanLiteral True)]

      it "should be able to assign a `False`" $ do
        let text = "value = False"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [Statement $ AssignValue (BindingName "value") (BooleanLiteral False)]

      it "should be able to assign another binding" $ do
        let text = "value2 = \"hello\"\nvalue = value2"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [ Statement $ AssignValue (BindingName "value2") (StringLiteral "hello"),
              Statement $ AssignValue (BindingName "value") (BindingExpression $ BindingName "value2")
            ]

      it "should be able to assign a binding starting with 'true'" $ do
        let text = "truest = \"hello\"\nvalue = truest"
        result <- parseScript (Filename "test.glue") text
        result
          `shouldBe` Right
            [ Statement $ AssignValue (BindingName "truest") (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "value")
                  (BindingExpression $ BindingName "truest")
            ]

      it "should not be able to assign a binding starting with 'true' that is not defined" $ do
        let text = "value = truest"
        result <- parseScript (Filename "test.glue") text
        isLeft result `shouldBe` True

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

      it "should be able to assign a shell command with text and string interpolation" $ do
        let text = "test = \"hello\"\nvalue = 'echo `test {test}`'"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $ AssignValue (BindingName "test") (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "value")
                  ( ShellCommand
                      [ ShellCommandLiteral "echo ",
                        ShellCommandInterpolation
                          [ LiteralFragment "test ",
                            BindingFragment (BindingName "test")
                          ]
                      ]
                      Nothing
                  )
            ]
      it "should be able to interpolate several bound variables in a shell command" $ do
        let text = "output = \"hello\"\nerror = \"there\"\nexitCode = 0\n'echo `Output: {output} | Error: {error} | Exit code: {exitCode}`'"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "output")
                  (StringLiteral "hello"),
              Statement $
                AssignValue
                  (BindingName "error")
                  (StringLiteral "there"),
              Statement $
                AssignValue
                  (BindingName "exitCode")
                  (IntegerLiteral 0),
              Expression $
                ( ShellCommand
                    [ ShellCommandLiteral "echo ",
                      ShellCommandInterpolation
                        [ LiteralFragment "Output: ",
                          BindingFragment (BindingName "output"),
                          LiteralFragment " | Error: ",
                          BindingFragment (BindingName "error"),
                          LiteralFragment " | Exit code: ",
                          BindingFragment (BindingName "exitCode")
                        ]
                    ]
                    Nothing
                )
            ]

    describe "if statements" $ do
      it "should be able to parse a basic if statement with a boolean literal" $ do
        let text = "if True { value = \"test\" } else {}"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                IfStatement
                  (BooleanLiteral True)
                  [Statement $ AssignValue (BindingName "value") (StringLiteral "test")]
                  []
            ]

      it "should be able to parse a basic if statement with a binding expression" $ do
        let text = "test = 'echo test'\nif test { value = \"test\" } else { value = \"test2\" }"
        result <- parseScript (Filename "test.glue") text
        expectRight result
        result
          `shouldBe` Right
            [ Statement $
                AssignValue
                  (BindingName "test")
                  (ShellCommand [ShellCommandLiteral "echo test"] Nothing),
              Statement $
                IfStatement
                  (BindingExpression (BindingName "test"))
                  [Statement $ AssignValue (BindingName "value") (StringLiteral "test")]
                  [Statement $ AssignValue (BindingName "value") (StringLiteral "test2")]
            ]

    describe "full test" $ do
      it "should be able to parse test file" $ do
        textContent <- readFileUtf8 testFile
        result <- parseScript (Filename testFile) textContent
        expectRight result
        length <$> result `shouldBe` Right 8
        result
          `shouldBe` Right
            [ Statement $ AssignValue (BindingName "user") (StringLiteral "pesho"),
              Statement $
                AssignValue
                  (BindingName "result")
                  (ShellCommand [ShellCommandLiteral "ls -l"] Nothing),
              Statement $
                AssignValue
                  (BindingName "output")
                  (ShellCommand [ShellCommandLiteral "ls -l"] (Just ShellStandardOut)),
              Statement $
                AssignValue
                  (BindingName "error")
                  (ShellCommand [ShellCommandLiteral "ls -l"] (Just ShellStandardError)),
              Statement $
                AssignValue
                  (BindingName "exitCode")
                  (ShellCommand [ShellCommandLiteral "ls -l"] (Just ShellExitCode)),
              Statement $
                IfStatement
                  (BindingExpression $ BindingName "result")
                  [Statement $ AssignValue (BindingName "outputString") (StringLiteral "Success!")]
                  [Statement $ AssignValue (BindingName "outputString") (StringLiteral "Failure!")],
              Expression $
                ShellCommand
                  [ ShellCommandLiteral "echo ",
                    ShellCommandInterpolation [BindingFragment (BindingName "outputString")]
                  ]
                  Nothing,
              Expression $
                ( ShellCommand
                    [ ShellCommandLiteral "echo ",
                      ShellCommandInterpolation
                        [ LiteralFragment "Output: ",
                          BindingFragment (BindingName "output"),
                          LiteralFragment " | Error: ",
                          BindingFragment (BindingName "error"),
                          LiteralFragment " | Exit code: ",
                          BindingFragment (BindingName "exitCode")
                        ]
                    ]
                    Nothing
                )
            ]

expectRight :: Either (ParseErrorBundle Text Void) a -> Expectation
expectRight (Right _) = return ()
expectRight (Left e) = expectationFailure $ errorBundlePretty e
