module CompileTest where

import  Lib                 (Expr (Lit, Term, Abs, App), Term(In), compile)
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
    lit = In $ Lit "x"
    term = In $ Term "x"
    abs_id = In $ Abs "x" term
    app_id = In $ App abs_id (In $ Lit "42")
    abs_const = In $ Abs "x" $ In $ Abs "y" $ In $ Term "x"
    app_const = In $ App (In $ App abs_const (In (Lit "1"))) $ In $ Lit "0"
