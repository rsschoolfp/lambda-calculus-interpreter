{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

-- import Prelude (Eq((==), (/=)), Show(show), String, Bool(True), ($), (++), (.))
import Prelude (Eq((==), (/=)), Show(show), String, Int, ($), (++), (.), (+), const, flip)
import Data.Functor (Functor(fmap))
import Data.Bool (Bool(True), otherwise, (&&), (||), not)
-- import Data.List (elem, concatMap, (!!), lookup)
-- import Data.Maybe (Maybe(Just, Nothing))
import Text.Printf (printf)

type Identifier = String

data ErrIdentifier = ErrIdentifier String deriving (Eq)

instance Show ErrIdentifier where
  show (ErrIdentifier arg) = "\x1b[31m\"" ++ arg ++ "\"\x1b[0m"

-- type Env = [(Identifier, Value)]

data Expr a
  = Lit String
  | Term Identifier
  | Abs Identifier a
  | App a a
  deriving (Show, Eq, Functor)

data Term f = In { out :: f (Term f) }
deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

bottomUp, topDown :: Functor f => (Term f -> Term f) -> Term f -> Term f
-- bottomUp :: (a -> b) -> c -> d
-- c = Term f
-- bottomUp :: (a -> b) -> Term f -> d
-- out (Term f) = f (Term f)
-- fmap (bottomUp fn) ::> f Term f -> f ?
-- bottomUp fn ::> Term f -> ?
  -- with In:
  -- In: f (Term f) -> Term f
  -- f ? ::> f (Term f)
  -- ? ::> Term f

  -- without In
  -- fn :: ? -> d
  -- bottomUp fn :: Term f -> d
  -- fmap (bottomUp fn) :: f (Term f) -> f d
  -- ? ::> f d
  -- fn ::> f d -> d

bottomUp fn = cata (fn . In)
topDown fn  = In . fmap (topDown fn) . out . fn

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata alg = alg . fmap (cata alg) . out

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Term f
ana coalg = In . fmap (ana coalg) . coalg

countNodes :: Term Expr -> Int
countNodes = cata count
  where count (Abs _ expr) = 1 + expr
        count (App t u) = 1 + t + u
        count _         = 1

prettyPrint :: Term Expr -> String
prettyPrint = cata stringify
  where stringify (Lit string)      = "\"" ++ string ++ "\""
        stringify (Term identifier) = identifier
        stringify (Abs arg expr)    = "λ" ++ arg ++ "." ++ expr
        stringify (App t u)         = "(" ++ t ++ ")(" ++ u ++ ")"

type RAlgebra f a = f (Term f, a) -> a

para :: Functor f => RAlgebra f a -> Term f -> a
para rAlg = rAlg . fmap fanout . out
  where fanout t = (t, para rAlg t)

type RAlgebra' f a = Term f -> f a -> a

(&) :: a -> (a -> b) -> b
(&) = flip ($)

para' :: Functor f => RAlgebra' f a -> Term f -> a
para' rAlg t = out t & fmap (para' rAlg) & rAlg t

cata' :: Functor f => Algebra f a -> Term f -> a
cata' alg = para' $ const alg

-- data Value
--   = Value String
--   | Closure Expr Env Identifier
--   deriving (Show, Eq)
--
-- eval :: Env -> Expr -> Maybe Value
-- eval _   (Lit string)          = Just $ Value string
-- eval env (Term identifier)     = lookup identifier env
-- eval env (Abs identifier expr) = Just $ Closure expr env identifier
-- eval env (App t u) = do
--   vt <- eval env t
--   vu <- eval env u
--   case vt of
--     Closure expr env' arg -> eval ((arg, vu) : env') expr
--     _                     -> Nothing

-- data ShadowVar = ShadowVar Identifier Expr

-- instance Eq ShadowVar where
--   (==) (ShadowVar x _) (ShadowVar y _) = x == y
--
-- instance Show ShadowVar where
--   show (ShadowVar arg expr) = "\n\t" ++ show arg ++ "\x1b[36m in \x1b[0m{ " ++ show expr ++ " }\n"
--
-- checkShadowing :: [Identifier] -> Expr -> [ShadowVar]
-- checkShadowing args (Abs arg expr)
--   | arg !! 0 == '_' = nested
--   | elem arg args   = (ShadowVar arg expr) : nested
--   | otherwise       = nested
--     where
--       nested  = checkShadowing (arg : args) expr
-- checkShadowing args (App t u) = concatMap (checkShadowing args) [t, u]
-- checkShadowing _ _ = []

isFreeVarOf :: Identifier -> Term Expr -> Bool
isFreeVarOf var (In (Term t))         = var == t
isFreeVarOf var (In (Abs ident body)) = var /= ident && isFreeVarOf var body
isFreeVarOf var (In (App t u))        = isFreeVarOf var t || isFreeVarOf var u
isFreeVarOf _ _                       = True

etaReduce :: Term Expr -> Term Expr
etaReduce = bottomUp reduce
  where
    reduce term@(In
                  (Abs ident
                    (In
                      (App term'
                        (In
                          (Term ident')))))) | ident == ident' && (not $ isFreeVarOf ident term') = term'
                                             | otherwise = term
    reduce term = term


compile :: Term Expr -> String
compile = cata alg
  where alg (Lit string)          = printf "'%s'" string
        alg (Term identifier)     = identifier
        alg (Abs identifier expr) = printf "(%s => %s)" identifier expr
        alg (App t u)             = printf "%s(%s)" t u
