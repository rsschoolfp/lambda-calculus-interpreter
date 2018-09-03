module Main where

import Prelude (IO, pure)

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
main = pure ()
