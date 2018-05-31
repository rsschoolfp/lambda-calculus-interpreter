module Main where

import Prelude (IO, pure)

etaReduce :: Expr -> Expr
etaReduce expr = case expr of
  (Abs ident (App expr' (Term ident'))) ->
    if ident == ident' && isNotFreeVar then expr' else expr
      where isNotFreeVar = not $ isFreeVarOf ident expr'
  _                                     -> expr

testEtaReduce :: IO ()
testEtaReduce =
  print
    [ etaReduce (Abs "x" (App l_id (Term "x"))) == l_id
    , etaReduce (Abs "x" (App (App l_const (Term "z")) (Term "x"))) == App l_const (Term "z")
    , etaReduce not_reducable_1 == not_reducable_1
    , etaReduce not_reducable_2 == not_reducable_2
    , etaReduce not_reducable_3 == not_reducable_3
    , etaReduce not_reducable_4 == not_reducable_4
    ]
  where
    not_reducable_1 = Abs "x" (App (Abs "y" (Term "x")) (Term "x"))
    not_reducable_2 = Abs "x" (App (Abs "y" (Abs "z" (Term "x"))) (Term "x"))
    not_reducable_3 = Abs "x" (App (App l_const (Term "x")) (Term "x"))
    not_reducable_4 = Abs "x" (App (Lit "s") (Term "x"))

isFreeVarOf :: Identifier -> Expr -> Bool
isFreeVarOf var (Lit s) = True
isFreeVarOf var (Term t) = var == t
isFreeVarOf var (Abs ident body) = var /= ident && isFreeVarOf var body
isFreeVarOf var (App t u) = isFreeVarOf var t || isFreeVarOf var u

testIsFreeVarOf :: IO ()
testIsFreeVarOf =
  print
    [ (isFreeVarOf "x" $ Abs "x" (Term "x"), False)
    , (isFreeVarOf "x" $ Abs "y" (Term "x"), True)
    , (isFreeVarOf "x" $ Abs "x" (App (Abs "x" (Term "x")) (Term "x")), False)
    , (isFreeVarOf "z" $ Abs "x" (App (Abs "y" (Term "z")) (Term "x")), True)
    ]

main :: IO ()
main = pure ()
