
module Main where


import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Text.PrettyPrint.HughesPJ
import qualified Semgrep.Languages.C as C
import qualified Semgrep.Languages.Python as P
import           Data.Generics
import           Debug.Trace
import           System.Environment
import           Data.List.Utils

import           Semgrep.Languages.Generic
import           Semgrep


isIn = contains


allIncludes :: [Option] -> [String]
allIncludes opts = ['-':'I':s | (Include s) <- opts]


pyVer :: [Option] -> PythonVersion
pyVer opts = let maybePv = [pv | (PythonVersion pv) <- opts]
             in case maybePv of
                  a:_ -> a
                  []  -> Python2


getParser :: FilePath -> [Option] -> Maybe LanguageParser
getParser f opts
  | ".c" `isIn` f  = Just $ flip C.parse (allIncludes opts)
  | ".py" `isIn` f = Just $ P.parse (pyVer opts)
  | otherwise      = Nothing



process file = do
  let maybeParsed = (getParser file []) <*> Just file
  case maybeParsed of
    Nothing -> print $ "No parser found for " ++ file
    Just e  -> do
      parsed <- e
      case parsed of
        Left msg  -> print msg
        Right project -> do
          let all = exprs project
          let conds = conditions all

          putStrLn "\nAll Expressions"
          mapM_ print all
          putStrLn "\nConditional Expressions"
          mapM_ print conds
          putStrLn (show project)
    --    putStrLn "\nIf Statements"
    --    mapM_ prettyPrint ifs

main = do
  [file] <- getArgs
  process file
