module CompileTest where

import  Lib                 (Expr (Lit, Term, Abs, App), compile)
import  Test.Tasty          (TestTree, testGroup)
import  Test.Tasty.HUnit    (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "compile"
    [ testCase "Literal" $
        compile lit @?= "'x'"
    , testCase "Term" $
        compile term @?= "x"
    , testCase "App Abs id" $
        compile app_id @?= "(x => x)('42')"
    , testCase "App Abs const" $
        compile app_const @?= "(x => (y => x))('1')('0')"
    ]
  where
    lit = Lit "x"
    term = Term "x"
    abs_id = Abs "x" term
    app_id = App abs_id (Lit "42")
    abs_const = Abs "x" $ Abs "y" $ Term "x"
    app_const = App (App abs_const $ Lit "1") $ Lit "0"
