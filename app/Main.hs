module Main where

import Data.List (nub, delete, (\\))
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

-- var can be unused at all or shadowed
-- ignore identifier "_"
checkUnused :: Expr -> [Identifier]
checkUnused expr = declared \\ used
  where
    declared = delete "_" $ nub declared_dup
    used = nub used_dup
    (declared_dup, used_dup) = go ([], []) expr
    go env (Lit _) = env
    go (declared, used) (Term id) = (declared, id : used)
    go env (App expr1 expr2) = (mappend declared1 declared2, mappend used1 used2)
      where
        (declared1, used1) = go env expr1
        (declared2, used2) = go env expr2
    go (declared, used) (Abs id expr) = go (id : declared, used) expr

checkUnused_test :: IO Bool
checkUnused_test = do 
  checkUnused no_unused      `shouldBe` []
  checkUnused y_unused       `shouldBe` ["y"]
  checkUnused y_z_unused     `shouldBe` ["z", "y"]
  checkUnused x_unused       `shouldBe` ["x"]
  checkUnused ignored_unused `shouldBe` []
  where
    no_unused = Abs "x" $ Term "x"
    y_unused = Abs "x" $ Abs "y" $ Term "x"
    y_z_unused = Abs "x" $ Abs "y" $ Abs "z" $ Term "x"
    x_unused = Abs "x" $ Abs "y" $ App (Abs "z" $ Term "z") (Term "y")
    ignored_unused = Abs "x" $ Abs "_" $ Term "x"

shouldBe :: (Show a, Eq a) => a -> a -> IO Bool
shouldBe actual expected = 
  if actual == expected 
    then pure True
    else do
      putStrLn $ "Expected " ++ show expected ++ ", got: " ++ show actual
      pure False

main :: IO ()
main = do
  result <- checkUnused_test
  if result then
    putStrLn "All tests pass"
  else
    putStrLn "There was an error"
