module EtaReduceTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Expr(Lit, Term, Abs, App), Term(In), etaReduce)

tests :: TestTree
tests = testGroup "Eta Reduction" [etaReduceTests]

etaReduceTests :: TestTree
etaReduceTests =
  testGroup "etaReduce"
    [ testCase "Term with different identifier" $
        etaReduce (In (Abs "x" (In (App (In (Term "y")) (In (Term "x")))))) @?= (In (Term "y"))
    , testCase "Term with same identifier" $
        etaReduce not_reducable_1 @?= not_reducable_1
    , testCase "Abs without free var" $
        etaReduce (In (Abs "x" (In (App (In (Abs "x" (In (Term "x")))) (In (Term "x")))))) @?= In (Abs "x" (In (Term "x")))
    , testCase "Abs with free var" $
        etaReduce not_reducable_2 @?= not_reducable_2
    , testCase "Lit" $
        etaReduce not_reducable_3 @?= not_reducable_3
    ]
  where
    not_reducable_1 = In (Abs "x" (In (App (In (Term "x")) (In (Term "x")))))
    not_reducable_2 = In (Abs "x" (In (App (In (Abs "y" (In (Term "x")))) (In (Term "x")))))
    not_reducable_3 = In (Abs "x" (In (App (In (Lit "s")) (In (Term "x")))))

-- isFreeVarOfTests :: TestTree
-- isFreeVarOfTests =
--   testGroup "isFreeVarOf"
--     [ testCase "Lit" $
--         isFreeVarOf "x" lit @?= True
--     , testCase "Term with same identifier" $
--         isFreeVarOf "x" term_x @?= True
--     , testCase "Term with different identifier" $
--         isFreeVarOf "x" term_y @?= False
--     , testCase "Abs without free var" $
--         isFreeVarOf "x" abs_id @?= False
--     , testCase "Abs with free var" $
--         isFreeVarOf "x" abs_part_const @?= True
--     , testCase "Deep Abs without free var" $
--         isFreeVarOf "x" abs_deep_x @?= False
--     , testCase "Deep Abs with free var" $
--         isFreeVarOf "z" abs_deep_z @?= True
--     ]
--   where
--     lit = Lit "s"
--     term_x = Term "x"
--     term_y = Term "y"
--     abs_id = Abs "x" (Term "x")
--     abs_part_const = Abs "y" (Term "x")
--     abs_deep_x = Abs "x" (Abs "x" (Term "x"))
--     abs_deep_z = Abs "x" (Abs "y" (Term "z"))
