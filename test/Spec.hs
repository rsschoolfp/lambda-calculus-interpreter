import           Test.Tasty (defaultMain, testGroup)

import qualified EvalTest
import qualified ShadowingTest
import qualified EtaReduceTest

main :: IO ()
main = defaultMain $
  testGroup "all tests"
    [   EvalTest.tests
      , ShadowingTest.tests
      , EtaReduceTest.tests
    ]
