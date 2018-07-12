module BetaReduceTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Expr(Lit, Term, Abs, App), betaReduce)

tests :: TestTree
tests =
  testGroup "betaReduce"
    [ testCase "Application (identity)" $
        betaReduce [] expr_app @?= Just (Lit "*")
    , testCase "Application (const)" $
        betaReduce [] expr_const @?= Just (Lit "*")
    ]
  where
    expr_app = App (Abs "x" (Term "x")) (Lit "*")
    const' = Abs "x" (Abs "y" (Term "x"))
    expr_const = App (App (App const' (App const' (Lit "*"))) (Lit "!")) (Lit "!!")
