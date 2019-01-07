{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser where

import Control.Applicative (Alternative(empty, (<|>)), many, some)
import Control.Monad (guard)
import Control.Arrow (first)
import Data.Char (isLetter)

import Lib (Expr(Lit, Term, Abs, App, Const), Identifier)

data Parser s m a = Parser { runParser :: s -> m (a, s) }

instance (Functor m) => Functor (Parser s m) where
  fmap f (Parser p) = Parser $ \s -> first f <$> p s

instance (Monad m) => Applicative (Parser s m) where
  pure x = Parser $ \s -> pure (x, s)
  (<*>) parserFn parserA = do
    f <- parserFn
    a <- parserA
    pure $ f a

instance (Monad m) => Monad (Parser s m) where
  (Parser pa) >>= f = Parser $ \s -> do
    (a, s') <- pa s
    runParser (f a) s'

instance (Alternative m, Monad m) => Alternative (Parser s m) where
  empty = Parser $ const empty
  Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s

item :: (Alternative m) => Parser String m Char
item = Parser $ \case
  []     -> empty
  (x:xs) -> pure (x, xs)

satisfy :: (Alternative m, Monad m) => (Char -> Bool) -> Parser String m Char
satisfy predicate = do
  c <- item
  guard $ predicate c
  pure c

char :: (Alternative m, Monad m) => Char -> Parser String m Char
char = satisfy . (==)

lit :: (Alternative m, Monad m) => Parser String m Expr
lit = do
  char '\"'
  str <- many $ satisfy (/= '\"')
  char '\"'
  pure $ Lit str

identifier :: (Alternative m, Monad m) => Parser String m Identifier
identifier = some $ satisfy isLetter

term :: (Alternative m, Monad m) => Parser String m Expr
term = do
  many $ char '\n'
  Term <$> identifier

lambda :: (Alternative m, Monad m) => Parser String m Expr
lambda = do
  many $ char '\n'
  char '('
  char 'λ'
  char ' '
  arg <- identifier
  char ' '
  ex <- expr
  char ')'
  pure $ Abs arg ex

app :: (Alternative m, Monad m) => Parser String m Expr
app = do
  many $ char '\n'
  char '('
  t <- expr
  char ' '
  u <- expr
  char ')'
  pure $ App t u

constant :: (Alternative m, Monad m) => Parser String m Expr
constant = do
  many $ char '\n'
  alias <- many $ satisfy (/= ' ')
  char ' '
  char '='
  char ' '
  val <- expr
  char '\n'
  pure $ Const alias val

expr :: (Alternative m, Monad m) => Parser String m Expr
expr
  =   lit
  <|> constant
  <|> term
  <|> lambda
  <|> app
