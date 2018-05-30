import           Test.Tasty
import           Test.Tasty.HUnit

import qualified ShadowingTest

main :: IO ()
main = defaultMain $ 
  testGroup "all tests" 
    [ ShadowingTest.tests
    ]
