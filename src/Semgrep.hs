{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Semgrep (
  Option(..)
, PythonVersion(..)
) where


data PythonVersion = Python2 | Python3

data Option = Include String
            | PythonVersion PythonVersion

