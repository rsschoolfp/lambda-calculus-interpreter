module EvalTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Expr(Lit, Term, Abs, App), Value(Value, Closure), eval)

tests :: TestTree
tests =
  testGroup "eval"
    [ testCase "Literal" $
        eval [] lit @?= Just (Value "42")
    , testCase "Empty Term" $
        eval [] term @?= Nothing
    , testCase "Term with Env" $
        eval [("x", Value "2")] term @?= Just (Value "2")
    , testCase "Abs" $
        eval [] abs' @?= Just (Closure term [] "x")
    , testCase "App Abs id" $
        eval [] app_id @?= Just (Value "42")
    , testCase "App Abs const" $
        eval [] (App just_id just_42) @?= Just (Value "42")
    , testCase "Nested Expr" $
        eval [] app_const @?= Just (Value "const")
    ]
  where
    lit = Lit "42"
    term = Term "x"
    abs' = Abs "x" term
    abs_id = Abs "x" term
    app_id = App abs_id lit
    abs_const = Abs "x" $ Abs "_" term
    app_const = App (App abs_const $ Lit "const") lit
    just_id = App (App abs_const abs_id) lit
    just_42 = App (App abs_const lit) abs_id
