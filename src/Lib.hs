module Lib where

import Prelude (Eq((==), (/=)), Show(show), String, Bool(True), Either(Left, Right), ($), (++))
import Data.Bool (otherwise, (&&), (||), not)
import Data.List (elem, concatMap, (!!), lookup, delete, (\\))
import Data.Maybe (maybe)
import Text.Printf (printf)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad ((>=>))

import Utils (maybeToEither)

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

data Error
  = UndeclaredVar Identifier
  | NonFunctionApp Identifier
  | UnexpectedErr
  deriving (Eq)

instance Show Error where
    show (UndeclaredVar v) = "\x1b[31mError: Undeclared identifier \"" ++ v ++ "\"\x1b[0m"
    show (NonFunctionApp v) = "\x1b[31mError: \"" ++ v ++ "\" is not callable\x1b[0m"
    show (UnexpectedErr) = "Unexpected Error"

eval :: Env Value -> Expr -> Either Error Value
eval _   (Lit string)          = Right $ Value string
eval env (Term identifier)     = maybeToEither (UndeclaredVar identifier) $ lookup identifier env
eval env (Abs identifier expr) = Right $ Closure expr env identifier
eval env (App t u) = do
  vt <- eval env t
  vu <- eval env u
  case vt of
    Closure expr env' arg -> eval ((arg, vu) : env') expr
    Value identifier      -> Left $ NonFunctionApp identifier

betaReduce :: Env Expr -> Expr -> Either Error Expr
betaReduce _   (Lit string) = Right $ Lit string
betaReduce env term@(Term identifier) = maybe (Right term) Right $ lookup identifier env
betaReduce env (Abs identifier expr) = Abs identifier <$> betaReduce env expr
betaReduce env (App t u) = do
  vt <- betaReduce env t
  vu <- betaReduce env u
  case vt of
    Abs arg expr -> betaReduce ((arg, vu) : env) expr
    _            -> Left UnexpectedErr


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

compile :: Expr -> Either Error String
compile (Lit string)          = Right $ printf "'%s'" string
compile (Term identifier)     = Right $ identifier
compile (Abs identifier expr) = printf "(%s => %s)" identifier <$> compile expr
compile (App (Lit id) _)      = Left $ NonFunctionApp id
compile (App t u)             = printf "%s(%s)" <$> compile t <*> compile u

compile' :: Expr -> Either Error String
compile' = betaReduce [] >=> compile
