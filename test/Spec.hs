import           Test.Tasty
import           Test.Tasty.HUnit

import qualified EvalTest
import qualified ShadowingTest

main :: IO ()
main = defaultMain $ 
  testGroup "all tests" 
    [   EvalTest.tests
      , ShadowingTest.tests
    ]
