module Main where

import Lib

type Identifier = String

data ErrIdentifier = ErrIdentifier String

instance Show ErrIdentifier where
  show (ErrIdentifier arg) = "\x1b[31m\"" ++ arg ++ "\"\x1b[0m"

type Env = [(Identifier, Value)]

data Expr
  = Lit String
  | Term Identifier
  | Abs Identifier Expr
  | ErrAbs ErrIdentifier Expr
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

data ShadowVar = ShadowVar Identifier Expr

instance Show ShadowVar where
  show (ShadowVar arg expr) = "\n\t" ++ show arg ++ "\x1b[36m in \x1b[0m{ " ++ show expr ++ " }\n"

checkShadowing :: [Identifier] -> Expr -> [ShadowVar]
checkShadowing args (Abs arg expr) 
  | arg !! 0 == '_' = nested
  | elem arg args   = ShadowVar arg (ErrAbs (ErrIdentifier arg) expr) : nested
  | otherwise       = nested
    where 
      nested  = checkShadowing (arg : args) expr
checkShadowing args (App t u) = concatMap (checkShadowing args) [t, u]
checkShadowing _ _ = []

testShadowing :: IO ()
testShadowing =
  print $ 
    [
      checkShadowing ["x"]  lit                   -- []
    , checkShadowing ["x"]  term                  -- []
    , checkShadowing ["_x"] abs_with_underscore   -- []
    , checkShadowing []     abs_twice_shadow      -- ["y", "x"]
    , checkShadowing ["x"]  abs_twice_shadow      -- ["x", "y", "x"]
    , checkShadowing []     app_twice_shadow      -- ["y", "x"]
    ]
  where
    lit = Lit "x"
    term = Term "x"
    abs_with_underscore = Abs "x" $ Abs "_" $ Abs "_" $ Abs "_x" $ Term "x"
    abs_twice_shadow = Abs "x" $ Abs "y" $ Abs "y" $ Abs "x" $ Term "x"
    app_twice_shadow = App (App (App (App abs_twice_shadow $ Lit "1") $ Lit "2") $ Lit "3") $ Lit "4"

main :: IO ()
main = someFunc
