module CheckUnusedTest where

import           Lib                                      ( Expr
                                                            ( Lit
                                                            , Term
                                                            , Abs
                                                            , App
                                                            )
                                                          , checkUnused
                                                          , mergeUnused
                                                          )
import  Test.Tasty          (TestTree, testGroup)
import  Test.Tasty.HUnit    (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "checkUnused" $
    checkUnusedTest ++ [mergeUnusedTest]

checkUnusedTest :: [TestTree]
checkUnusedTest = 
    [ testCase "No unused" $
        checkUnused no_unused @?= []
    , testCase "Unused y" $
        checkUnused y_unused @?= ["y"]
    , testCase "Two variables unused" $
        checkUnused y_z_unused @?= ["z", "y"]
    , testCase "Three variables, but one unused" $
        checkUnused x_unused @?= ["x"]
    , testCase "Ignore _" $
        checkUnused ignored_unused @?= []
    , testCase "Shadowing - one unused" $
        checkUnused shadowed_x @?= ["x"]
    , testCase "Shadowing - both unused" $
        checkUnused shadowed_unused @?= ["x", "x"]
    ]
  where
    no_unused = Abs "x" $ Term "x"
    y_unused = Abs "x" $ Abs "y" $ Term "x"
    y_z_unused = Abs "x" $ Abs "y" $ Abs "z" $ Term "x"
    x_unused = Abs "x" $ Abs "y" $ App (Abs "z" $ Term "z") (Term "y")
    ignored_unused = Abs "x" $ Abs "_" $ Term "x"
    shadowed_x = Abs "x" $ Abs "x" $ Term "x"
    shadowed_unused = Abs "x" $ App (Abs "x" $ Lit "hello") (Lit "world")

mergeUnusedTest :: TestTree
mergeUnusedTest = testGroup "mergeUnused"
  [ testCase "x is confirmed, y is not, and z is new" $
      mergeUnused ["x", "y"] ["x", "z"] ["x", "y"] @?= ["x", "z"]
  , testCase "two x are not confirmed, y and z are new" $ 
      mergeUnused ["x", "x"] ["x", "z"] ["x", "y"] @?= ["z", "y"]
  , testCase "two x, y and z are new" $ 
      mergeUnused [] ["x", "z"] ["x", "y"] @?= ["x", "z", "x", "y"]
  , testCase "y and x are not confirmed" $ 
      mergeUnused ["x", "y"] [] ["x", "y"] @?= []
  , testCase "x is cofirmed, y is not" $ 
      mergeUnused ["x", "y"] ["x"] ["x"] @?= ["x"]
  ]

