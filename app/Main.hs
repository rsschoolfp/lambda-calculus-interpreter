module Main where

import Data.List (delete, (\\))
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

checkUnused :: Expr -> [Identifier]
checkUnused expr = go [] expr
  where
    go unused (Lit _) = unused
    go unused (Term id) = delete id unused
    go unused (App expr_l expr_r) = mergeUnused unused (go unused expr_l) (go unused expr_r)
    go unused (Abs "_" expr) = go unused expr
    go unused (Abs id expr) = go (id : unused) expr

mergeUnused :: (Eq a) => [a] -> [a] -> [a] -> [a]
mergeUnused env left right = (env \\ removed) ++ added
  where
    removed = (env \\ left) ++ (env \\ right)
    added = (left \\ env) ++ (right \\ env)
  
mergeUnused_test :: IO ()
mergeUnused_test = do
  mergeUnused ["x", "y"] ["x", "z"] ["x", "y"] `shouldBe` ["x", "z"]
  mergeUnused ["x", "x"] ["x", "z"] ["x", "y"] `shouldBe` ["z", "y"]
  mergeUnused [] ["x", "z"] ["x", "y"] `shouldBe` ["x", "z", "x", "y"]
  mergeUnused ["x", "y"] [] ["x", "y"] `shouldBe` []
  mergeUnused ["x", "y"] ["x"] ["x"] `shouldBe` ["x"]

checkUnused_test :: IO ()
checkUnused_test = do 
  checkUnused no_unused       `shouldBe` []
  checkUnused y_unused        `shouldBe` ["y"]
  checkUnused y_z_unused      `shouldBe` ["z", "y"]
  checkUnused x_unused        `shouldBe` ["x"]
  checkUnused ignored_unused  `shouldBe` []
  checkUnused shadowed_x      `shouldBe` ["x"]
  checkUnused shadowed_unused `shouldBe` ["x", "x"]
  where
    no_unused = Abs "x" $ Term "x"
    y_unused = Abs "x" $ Abs "y" $ Term "x"
    y_z_unused = Abs "x" $ Abs "y" $ Abs "z" $ Term "x"
    x_unused = Abs "x" $ Abs "y" $ App (Abs "z" $ Term "z") (Term "y")
    ignored_unused = Abs "x" $ Abs "_" $ Term "x"
    shadowed_x = Abs "x" $ Abs "x" $ Term "x"
    shadowed_unused = Abs "x" $ App (Abs "x" $ Lit "hello") (Lit "world")

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = 
  if actual == expected 
    then pure ()
    else do
      putStrLn $ "Expected " ++ show expected ++ ", got: " ++ show actual

main :: IO ()
main = do
  checkUnused_test
  mergeUnused_test
