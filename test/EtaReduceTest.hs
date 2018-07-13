module EtaReduceTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Expr(Lit, Term, Abs, App), etaReduce, isFreeVarOf)

tests :: TestTree
tests = testGroup "Eta Reduction" [etaReduceTests, isFreeVarOfTests]

etaReduceTests :: TestTree
etaReduceTests =
  testGroup "etaReduce"
    [ testCase "Term with different identifier" $
        etaReduce (Abs "x" (App (Term "y") (Term "x"))) @?= (Term "y")
    , testCase "Term with same identifier" $
        etaReduce not_reducable_1 @?= not_reducable_1
    , testCase "Abs without free var" $
        etaReduce (Abs "x" (App (Abs "x" (Term "x")) (Term "x"))) @?= Abs "x" (Term "x")
    , testCase "Abs with free var" $
        etaReduce not_reducable_2 @?= not_reducable_2
    , testCase "Lit" $
        etaReduce not_reducable_3 @?= not_reducable_3
    , testCase "" $
        etaReduce (Abs "x" (App (Abs "z" (App (Term "y") (Term "z"))) (Term "x"))) @?= (Term "y")
    ]
  where
    not_reducable_1 = Abs "x" (App (Term "x") (Term "x"))
    not_reducable_2 = Abs "x" (App (Abs "y" (Term "x")) (Term "x"))
    not_reducable_3 = Abs "x" (App (Lit "s") (Term "x"))

isFreeVarOfTests :: TestTree
isFreeVarOfTests =
  testGroup "isFreeVarOf"
    [ testCase "Lit" $
        isFreeVarOf "x" lit @?= True
    , testCase "Term with same identifier" $
        isFreeVarOf "x" term_x @?= True
    , testCase "Term with different identifier" $
        isFreeVarOf "x" term_y @?= False
    , testCase "Abs without free var" $
        isFreeVarOf "x" abs_id @?= False
    , testCase "Abs with free var" $
        isFreeVarOf "x" abs_part_const @?= True
    , testCase "Deep Abs without free var" $
        isFreeVarOf "x" abs_deep_x @?= False
    , testCase "Deep Abs with free var" $
        isFreeVarOf "z" abs_deep_z @?= True
    ]
  where
    lit = Lit "s"
    term_x = Term "x"
    term_y = Term "y"
    abs_id = Abs "x" (Term "x")
    abs_part_const = Abs "y" (Term "x")
    abs_deep_x = Abs "x" (Abs "x" (Term "x"))
    abs_deep_z = Abs "x" (Abs "y" (Term "z"))
