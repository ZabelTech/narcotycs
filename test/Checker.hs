module Checker where

import Test.HUnit
import Naive.Parser
import Naive.Checker
import Naive.Constraints

instance Eq CheckerError where
  a == b = show a == show b

assertCorrectlyParsed code =
  either (assertFailure . ("parsing error: " ++) . show) return $ Naive.Parser.parseString code

assertCollectionError code expectedError = TestCase $ do
  ast <- assertCorrectlyParsed code
  assertEqual code (Just expectedError) $ Naive.Checker.checkCode ast

assertConstraintError code expectedError = TestCase $ do
  ast <- assertCorrectlyParsed code
  assertEqual code (Just (Constraint expectedError)) $ Naive.Checker.checkCode ast

assertSuccess code = TestCase $ do
  ast <- assertCorrectlyParsed code
  assertEqual code Nothing $ Naive.Checker.checkCode ast

collectionErrors = TestList [
  "unknownVar" `assertCollectionError` UnknownIdentifier "unknownVar",
  "get(\"foo\")" `assertCollectionError` WrongArity 2 1
  ]

constraintErrors = TestList [
  "get(\"foo\",\"foo\")" `assertConstraintError` (UnexpectedType String Index),

  "get({},\"foo\")"
  `assertConstraintError`
  (NotSatisfied [HasKey (By (IsLiteral "foo"))]),

  "get({\"bar\": \"\", \"baz\": \"\"},\"foo\")"
  `assertConstraintError`
  (NotSatisfied [HasKey (By (IsLiteral "foo"))]),

  "add({\"foo\": \"bar\"},\"foo\",\"\")"
  `assertConstraintError`
  (Conflicts [(HasKey (By (IsLiteral "foo")),
               HasNotKey (By (IsLiteral "foo")))]),

  "var = del({\"foo\": \"bar\", \"baz\": \"\"},\"baz\") \n get(var,\"baz\")"
  `assertConstraintError`
  (Conflicts [(HasKey (By (IsLiteral "baz")),
               HasNotKey (By (IsLiteral "baz")))])

  ]

success = TestList [
  assertSuccess "newVar = {}",
  assertSuccess "get({\"foo\": \"bar\"},\"foo\")",
  assertSuccess "add({\"foo\": \"bar\"},\"baz\",\"\")",
  assertSuccess "var = add({\"foo\": \"bar\"},\"baz\",\"\") \n get(var,\"foo\")",
  assertSuccess "var = add({\"foo\": \"bar\", \"toto\": \"\"},\"baz\",\"\") \n get(var,\"foo\")",
  assertSuccess "var = add({\"foo\": \"bar\"},\"baz\",\"\") \n get(var,\"baz\")",
  assertSuccess "var = del({\"foo\": \"bar\", \"baz\": \"\"},\"foo\") \n get(var,\"baz\")"
  ]

tests = TestList [TestLabel "Collection Errors" collectionErrors,
                  TestLabel "Unsatisfied" constraintErrors,
                  TestLabel "Success" success
                 ]
