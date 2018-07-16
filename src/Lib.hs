module Lib where

import Prelude (Eq((==), (/=)), Show(show), String, Bool(True), ($), (++), (.))
import Data.Bool (otherwise, (&&), (||), not)
import Data.List (elem, concatMap, (!!), lookup, delete, (\\))
import Data.Maybe (Maybe(Just, Nothing))
import Text.Printf (printf)
import Data.Functor ((<$>))
import Control.Applicative (pure, (<|>))
import Control.Monad ((>=>))

type Identifier = String

newtype ErrIdentifier = ErrIdentifier String deriving (Eq)

instance Show ErrIdentifier where
  show (ErrIdentifier arg) = "\x1b[31m\"" ++ arg ++ "\"\x1b[0m"

type Env a = [(Identifier, a)]

data Expr
  = Lit String
  | Term Identifier
  | Abs Identifier Expr
  | App Expr Expr
  deriving (Show, Eq)

data Value
  = Value String
  | Closure Expr (Env Value) Identifier
  deriving (Show, Eq)

eval :: Env Value -> Expr -> Maybe Value
eval _   (Lit string)          = Just $ Value string
eval env (Term identifier)     = lookup identifier env
eval env (Abs identifier expr) = Just $ Closure expr env identifier
eval env (App t u) = do
  vt <- eval env t
  vu <- eval env u
  case vt of
    Closure expr env' arg -> eval ((arg, vu) : env') expr
    _                     -> Nothing

betaReduce :: Env Expr -> Expr -> Maybe Expr
betaReduce _   (Lit string) = Just $ Lit string
betaReduce env term@(Term identifier) = lookup identifier env <|> Just term
betaReduce env (Abs identifier expr) = Abs identifier <$> betaReduce env expr
betaReduce env (App t u) = do
  vt <- betaReduce env t
  vu <- betaReduce env u
  case vt of
    Abs arg expr -> betaReduce ((arg, vu) : env) expr
    _            -> Nothing


data ShadowVar = ShadowVar ErrIdentifier Expr

instance Eq ShadowVar where
  (==) (ShadowVar x _) (ShadowVar y _) = x == y

instance Show ShadowVar where
  show (ShadowVar arg expr) = "\n\t" ++ show arg ++ "\x1b[36m in \x1b[0m{ " ++ show expr ++ " }\n"

checkShadowing :: [Identifier] -> Expr -> [ShadowVar]
checkShadowing args (Abs arg expr)
  | arg !! 0 == '_' = nested
  | elem arg args   = ShadowVar (ErrIdentifier arg) expr : nested
  | otherwise       = nested
    where
      nested  = checkShadowing (arg : args) expr
checkShadowing args (App t u) = concatMap (checkShadowing args) [t, u]
checkShadowing _ _ = []

checkUnused :: Expr -> [Identifier]
checkUnused = go []
  where
    go unused (Lit _) = unused
    go unused (Term id) = delete id unused
    go unused (App expr_l expr_r) =
      mergeUnused unused (go unused expr_l) (go unused expr_r)
    go unused (Abs "_" expr) = go unused expr
    go unused (Abs id expr) = go (id : unused) expr

mergeUnused :: (Eq a) => [a] -> [a] -> [a] -> [a]
mergeUnused env left right = (env \\ removed) ++ added
  where
    removed = (env \\ left) ++ (env \\ right)
    added = (left \\ env) ++ (right \\ env)

etaReduce :: Expr -> Expr
etaReduce expr@(Abs ident (App expr' (Term ident'))) =
    if ident == ident' && isNotFreeVar then etaReduce expr' else expr
      where isNotFreeVar = not $ isFreeVarOf ident expr'
etaReduce expr = expr

isFreeVarOf :: Identifier -> Expr -> Bool
isFreeVarOf var (Term t)         = var == t
isFreeVarOf var (Abs ident body) = var /= ident && isFreeVarOf var body
isFreeVarOf var (App t u)        = isFreeVarOf var t || isFreeVarOf var u
isFreeVarOf _ _                  = True

compile :: Expr -> String
compile (Lit string)          = printf "'%s'" string
compile (Term identifier)     = identifier
compile (Abs identifier expr) = printf "(%s => %s)" identifier $ compile expr
compile (App t u)             = printf "%s(%s)" (compile t) (compile u)

compile' :: Expr -> Maybe String
compile' = betaReduce [] >=> pure . compile
