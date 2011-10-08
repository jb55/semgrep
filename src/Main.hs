
module Main where


import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Text.PrettyPrint.HughesPJ
import qualified Semgrep.Languages.C as C
import           Data.Generics
import           Debug.Trace
import           System.Environment

import           Semgrep.Languages.Generic

process file = do
  parsed <- C.parse file
  case parsed of
    Nothing  -> print "Failed to parse file"
    Just ast -> do
      let all = exprs ast
      let conds = conditions all

      putStrLn "\nAll Expressions"
      mapM_ print all
      putStrLn "\nConditional Expressions"
      mapM_ print conds
--    putStrLn "\nIf Statements"
--    mapM_ prettyPrint ifs

main = do
  [file] <- getArgs
  process file
