{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Semgrep (
  Annotated(..)
, Position(..)
, NodeInfo(..)
, strip
) where

import           Data.Generics

data Annotated a b = Annotated a b
                   deriving (Show, Typeable, Data, Functor)

strip :: Annotated a b -> b
strip (Annotated _ b) = b

data Position = Position
              { posOffset :: Maybe Int
              , posFilename :: Maybe String
              , posLineNumber :: Maybe Int
              , posColumnNumber :: Maybe Int
              }

try' :: (Show a) => Maybe a -> String
try' = maybe "??" show

try :: Maybe String -> String -> String
try m s = maybe s id m

instance Show Position where
  show (Position o f l c) =
    try f "Unknown File" ++ ": (" ++ try' l ++ ", " ++ try' c ++ ")"


data NodeInfo = NodeInfo Position (Maybe String)

instance Show NodeInfo where
  show (NodeInfo p s) =
    "NodeInfo " ++ show p ++ maybe "" (\x -> " \"" ++ x ++ "\"") s
