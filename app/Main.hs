module Main where

import Lib

type Identifier = String

type Env = [(Identifier, Value)]

data Expr
  = Lit String
  | Term Identifier
  | App Expr Expr
  | Abs Identifier Expr
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
  = Value String Env
  | Closure Expr Env Identifier
  deriving (Show)

  -- | Lit String
  -- | Term Identifier
  -- | App Expr Expr
  -- | Abs Identifier Expr

eval :: Env -> Expr -> Maybe Value
eval env (Lit string)          = Just $ Value string env
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

checkShadowing :: Expr -> Maybe Identifier
checkShadowing expr = go [] expr
  where
    go _ (Lit _) = Nothing
    go vars (Term id) = Nothing
    go vars (App expr1 expr2) = mappend (go vars expr1) (go vars expr2)
    -- shadowing can only occur in 'Abs' expression, because that's the 
    -- only place where we create closures
    go vars (Abs id expr) =
      if (elem id vars)
        then Just id
        else go (id : vars) expr

checkShadowing_test :: IO ()
checkShadowing_test = do 
  print $ checkShadowing shadowed_expr
  print $ checkShadowing non_shadowed_expr
  where
    shadowed_expr = Abs "x" $ Abs "x" $ Term "x"
    non_shadowed_expr = Abs "y" $ Abs "x" $ Term "x"

main :: IO ()
main = do
  checkShadowing_test
