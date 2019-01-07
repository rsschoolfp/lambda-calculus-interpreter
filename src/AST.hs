{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module AST where

import Control.Applicative (Alternative, some)
import Control.Monad (guard)

import Lib (Expr, Identifier, Error, betaReduce, compile)
import Parser (Parser, char, identifier, expr)

type Definition = (Identifier, Expr)

data AST = AST
  { defs :: [Definition]
  , main :: Expr
  } deriving (Show)

definition :: (Alternative m, Monad m) => Parser String m Definition
definition = do
  name <- identifier
  char ' '
  char '='
  char ' '
  ex <- expr
  pure (name, ex)

ast :: (Alternative m, Monad m) => Parser String m AST
ast = AST <$> some p <*> mainP
  where
    p = do
      def@(name, _) <- definition
      guard $ name /= "main"
      char '\n'
      pure def
    mainP = do
      def@(name, ex) <- definition
      guard $ name == "main"
      pure ex

compileAst :: AST -> Either Error String
compileAst (AST defs main) = betaReduce defs main >>= compile