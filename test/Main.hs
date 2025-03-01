import Test.HUnit

import qualified Parser
import qualified Checker

main = runTestTTAndExit $ TestList [Parser.tests, Checker.tests]
