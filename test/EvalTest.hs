module EvalTest where

import  Lib (Expr(..), Value(..), eval)
import  Test.Tasty
import  Test.Tasty.HUnit

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
        eval [] abs @?= Just (Closure term [] "x")
    , testCase "App Abs id" $
        eval [] app_id @?= Just (Value "42")
    , testCase "App Abs const" $
        eval [] (App just_id just_42) @?= Just (Value "42")
    , testCase "Nested Expr" $
        eval [] app_const @?= Just (Value "777")
    ]
  where
    lit = Lit "42"
    term = Term "x"
    abs = Abs "x" term
    abs_id = Abs "x" (Term "x")
    app_id = App abs_id lit
    abs_const = Abs "x" $ Abs "_" $ Term "x"
    app_const = App (App abs_const $ Lit "777") lit
    just_id = (App (App abs_const abs_id) lit)
    just_42 = (App (App abs_const lit) abs_id)
