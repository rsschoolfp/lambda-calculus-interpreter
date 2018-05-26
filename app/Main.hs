module Main where

import Lib

type Identifier = String

type Env = [(Identifier, Value)]

data Expr
  = Lit String
  | Term Identifier
  | Abs Identifier Expr
  | App Expr Expr
  deriving (Show)

l_id :: Expr
l_id = Abs "x" (Term "x")

l_const :: Expr
l_const = Abs "x" (Abs "y" (Term "x"))

test_1 :: Expr
test_1 = App l_id (Lit "wow")

test_2 :: Expr
test_2 = App (App l_const (Lit "*")) (Lit "!")

test_3 :: Expr
test_3 =
  App
    (App (App l_const l_id) (Lit "!"))
    (App (App l_const (Lit "*")) l_id)

test_4 :: Expr
test_4 =
  App l_id (App l_const (Lit "*"))

test_err_1 :: Expr
test_err_1 =
  App (App l_id (Lit "*")) (Lit "!")

data Value
  = Nill
  | Value String
  | Closure Expr Env Identifier
  deriving (Show)

eval :: Env -> Expr -> Maybe Value
eval env (Lit string)          = Just $ Value string
eval env (Term identifier)     = lookup identifier env
eval env (Abs identifier expr) = Just $ Closure expr env identifier
eval env (App t u) =
  let et = eval env t
      eu = eval env u
   in case et of
        Just (Closure expr env' arg) ->
          case eu of
            Nothing    -> Nothing
            Just value -> eval ((arg, value) : env') expr
        _                            -> Nothing


-- checkShadowing [] shadow_abs -> ["y","x"]
-- checkShadowing [("x", Nill)] shadow_abs -> ["x","y","x"]
shadow_abs :: Expr
shadow_abs = Abs "x" $ Abs "y" $ Abs "y" $ Abs "x" $ Term "x"

-- eval [] shadow_app -> Just (Value "4")
shadow_app :: Expr
shadow_app = App (App (App (App shadow_abs $ Lit "1") $ Lit "2") $ Lit "3") $ Lit "4"

checkShadowing :: Env -> Expr -> [Identifier]
checkShadowing env (Abs identifier expr) 
  | (length $ filter eq env) > 0 = identifier : nested
  | otherwise                    = nested
    where 
      eq (key, _) = key == identifier
      nested      = checkShadowing ((identifier, Nill) : env) expr
checkShadowing env (App t u) = concatMap (checkShadowing env) [t, u]
checkShadowing _ _           = []

main :: IO ()
main = someFunc
