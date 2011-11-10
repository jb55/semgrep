
module Main where


import           Control.Monad
import           Data.Maybe
import qualified Semgrep.Languages.C as C
import qualified Semgrep.Languages.Python as P
import           System.Environment
import           Data.List

import           Semgrep.Languages.Generic
import           Semgrep



allIncludes :: [Option] -> [String]
allIncludes opts = ['-':'I':s | (Include s) <- opts]


pyVer :: [Option] -> PythonVersion
pyVer opts = let maybePv = [pv | (PythonVersion pv) <- opts]
             in case maybePv of
                  a:_ -> a
                  []  -> Python2


getParser :: FilePath -> [Option] -> Maybe LanguageParser
getParser f opts
  | ".c" `isInfixOf` f  = Just $ flip C.parse (allIncludes opts)
  | ".py" `isInfixOf` f = Just $ P.parse (pyVer opts)
  | otherwise      = Nothing



process file = do
  let maybeParsed = do { p <- getParser file []; return $ p file }
  case maybeParsed of
    Nothing -> print $ "No parser found for " ++ file
    Just e  -> do
      parsed <- e
      case parsed of
        Left msg  -> print msg
        Right project -> do
          let all = exprs project
          let conds = conditions all
          let calls' = calls all

          putStrLn "\nAll Expressions"
          mapM_ print (map info all)
          putStrLn "\nConditional Expressions"
          mapM_ print (map info conds)
          putStrLn "\nFunction Calls"
          mapM_ print (map info calls')


    --    putStrLn (show project)
    --    putStrLn "\nIf Statements"
    --    mapM_ prettyPrint ifs

main = do
  [file] <- getArgs
  process file
