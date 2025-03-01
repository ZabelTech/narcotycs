module Parser where

import Data.Either (isLeft)
import Naive.Parser
import Test.HUnit

singleLine =
  parseString "get({},\"foo\")"
  ~?=
  (Right [Expression (Application "get" [DictLiteral [],StringLiteral "foo"])])

multipleLines =
  parseString "tbl = {} \n get(tbl,\"foo\")"
  ~?=
  (Right [Assignment "tbl" (DictLiteral []), Expression (Application "get" [Variable "tbl",StringLiteral "foo"])])

invalidAndEdgecases = TestList [
  isLeft (parseString "get(tbl,\"foo\"")    ~?= True,
  isLeft (parseString "get(tbl,\"foo)")     ~?= True,
  isLeft (parseString "get(tbl,\"foo\",)")  ~?= True,
  isLeft (parseString "get(tbl,\"foo\"\n)") ~?= False,
  isLeft (parseString "tbl = ")             ~?= True
  ]

tests = TestList [
  TestLabel "ParserTests" $
    TestList [singleLine,multipleLines,invalidAndEdgecases]
  ]
