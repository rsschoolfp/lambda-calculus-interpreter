module Lib where

import Prelude (Eq((==), (/=)), Show(show), String, Bool(True), ($), (++))
import Data.Bool (otherwise, (&&), (||), not)
import Data.List (elem, concatMap, (!!), lookup)
import Data.Maybe (Maybe(Just, Nothing))

type Identifier = String

data ErrIdentifier = ErrIdentifier String deriving (Eq)

instance Show ErrIdentifier where
  show (ErrIdentifier arg) = "\x1b[31m\"" ++ arg ++ "\"\x1b[0m"

type Env = [(Identifier, Value)]

data Expr
  = Lit String
  | Term Identifier
  | Abs Identifier Expr
  | ErrAbs ErrIdentifier Expr
  | App Expr Expr
  deriving (Show, Eq)

data Value
  = Value String
  | Closure Expr Env Identifier
  deriving (Show, Eq)

eval :: Env -> Expr -> Maybe Value
eval _   (Lit string)          = Just $ Value string
eval env (Term identifier)     = lookup identifier env
eval env (Abs identifier expr) = Just $ Closure expr env identifier
eval _   (ErrAbs _ _)          = Nothing
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

instance Eq ShadowVar where
  (==) (ShadowVar x _) (ShadowVar y _) = x == y

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

etaReduce :: Expr -> Expr
etaReduce expr@(Abs ident (App expr' (Term ident'))) =
    if ident == ident' && isNotFreeVar then expr' else expr
      where isNotFreeVar = not $ isFreeVarOf ident expr'
etaReduce expr = expr

isFreeVarOf :: Identifier -> Expr -> Bool
isFreeVarOf var (Term t)         = var == t
isFreeVarOf var (Abs ident body) = var /= ident && isFreeVarOf var body
isFreeVarOf var (App t u)        = isFreeVarOf var t || isFreeVarOf var u
isFreeVarOf _ _                  = True
