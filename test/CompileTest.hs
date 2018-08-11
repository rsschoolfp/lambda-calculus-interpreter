module CompileTest where

import  Lib                 (Expr(Lit, Term, Abs, App), Error(NonFunctionApp), compile)
import  Test.Tasty          (TestTree, testGroup)
import  Test.Tasty.HUnit    (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "compile"
    [ testCase "Literal" $
        compile lit @?= Right "'x'"
    , testCase "Term" $
        compile term @?= Right "x"
    , testCase "App Abs id" $
        compile app_id @?= Right "(x => x)('42')"
    , testCase "App Abs const" $
        compile app_const @?= Right "(x => (y => x))('1')('0')"
    , testCase "Non-function application error" $
        compile err_app @?= Left (NonFunctionApp "question")
    ]
  where
    lit = Lit "x"
    term = Term "x"
    abs_id = Abs "x" term
    app_id = App abs_id (Lit "42")
    abs_const = Abs "x" $ Abs "y" $ Term "x"
    app_const = App (App abs_const $ Lit "1") $ Lit "0"
    err_app = App (Lit "question") (Lit "42")
