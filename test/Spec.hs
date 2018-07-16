import           Test.Tasty (defaultMain, testGroup)

import qualified EvalTest
import qualified ShadowingTest
import qualified EtaReduceTest
import qualified CompileTest
import qualified BetaReduceTest
import qualified CheckUnusedTest

main :: IO ()
main = defaultMain $
  testGroup "all tests"
    [   EvalTest.tests
      , ShadowingTest.tests
      , EtaReduceTest.tests
      , CompileTest.tests
      , BetaReduceTest.tests
      , CheckUnusedTest.tests
    ]
