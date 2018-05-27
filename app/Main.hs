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
  = Value String
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

list_env :: Env -> [Identifier]
list_env []                = []
list_env [(key, _)]        = [key]
list_env ((key, _) : tail) = key : list_env tail

delete :: (Eq a) => a -> [a] -> [a]
delete _ []         = []
delete x (y : tail) = if x == y
                      then delete x tail
                      else y : delete x tail

into :: (Eq a) => a -> [a] -> Bool
into _ [] = False
into x (y : tail) = if x == y
                    then True
                    else into x tail

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x : tail) y = if into x y
                         then x : intersect tail y
                         else intersect tail y


unused :: Env -> Expr -> [Identifier]
unused env (Lit string)          = list_env env
unused env (Term identifier)     = delete identifier $ list_env env
unused env (Abs identifier expr) = delete identifier $ unused env expr
unused env (App t u)             = intersect (unused env t) (unused env u)

main :: IO ()
main = someFunc
