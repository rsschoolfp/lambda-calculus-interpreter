module ShadowingTest where

import  Lib                 (Expr (Lit, Term, Abs, App), ShadowVar (ShadowVar), ErrIdentifier(ErrIdentifier), checkShadowing)
import  Test.Tasty          (TestTree, testGroup)
import  Test.Tasty.HUnit    (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "checkShadowing"
    [ testCase "Literal" $
        checkShadowing ["x"] lit @?= []
    , testCase "Term" $
        checkShadowing ["x"] term @?= []
    , testCase "Abs with underscore" $
        checkShadowing ["_x"] abs_with_underscore @?= []
    , testCase "Abs twice shadow" $
        checkShadowing [] abs_twice_shadow @?=
          [ (ShadowVar (ErrIdentifier "y") lit)
          , (ShadowVar (ErrIdentifier "x") lit)
          ]
    , testCase "Abs twice shadow (one from env)" $
        checkShadowing ["x"] abs_twice_shadow @?=
          [ (ShadowVar (ErrIdentifier "x") lit)
          , (ShadowVar (ErrIdentifier "y") lit)
          , (ShadowVar (ErrIdentifier "x") lit)
          ]
    , testCase "App twice shadow" $
        checkShadowing [] app_twice_shadow @?=
          [ (ShadowVar (ErrIdentifier "y") lit)
          , (ShadowVar (ErrIdentifier "x") lit)
          ]
    ]
  where
    lit = Lit "x"
    term = Term "x"
    abs_with_underscore = Abs "x" $ Abs "_" $ Abs "_" $ Abs "_x" $ Term "x"
    abs_twice_shadow = Abs "x" $ Abs "y" $ Abs "y" $ Abs "x" $ Term "x"
    app_twice_shadow = App (App (App (App abs_twice_shadow $ Lit "1") $ Lit "2") $ Lit "3") $ Lit "4"
