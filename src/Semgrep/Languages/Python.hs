{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Python where

import qualified Language.Python.Common.AST as P
import           Language.Python.Common.SrcLocation
import           Language.Python.Version2.Parser as P2
import           Language.Python.Version3.Parser as P3
import           Language.Python.Common.Pretty
import           Language.Python.Common.PrettyAST
import           Semgrep.Languages.Generic
import           Data.Generics
import           System.IO
import           Semgrep ( PythonVersion(..)
                         , NodeInfo(..)
                         , Position(..)
                         , Annotated(..)
                         )


fromPyOp :: P.Op anno -> BinOp
fromPyOp (P.LessThan _)    = LeOp
fromPyOp (P.GreaterThan _) = GrOp
fromPyOp (P.Equality _)    = EqOp
fromPyOp o                 = UnkOp "dunno"

makePos :: String -> Int -> Int -> Position
makePos f r c = Position Nothing (Just f) (Just r) (Just c)

makeNode :: (Pretty n) => String -> Int -> Int -> n -> NodeInfo
makeNode f r c n = NodeInfo (makePos f r c) (Just $ show $ pretty n)

-- Build a NodeInfo given a pretty printable node and SrcLocation
fromLoc :: (Pretty n) => n -> SrcSpan -> Maybe NodeInfo
fromLoc n (SpanCoLinear f r c _)    = Just $ makeNode f r c n
fromLoc n (SpanMultiLine f r c _ _) = Just $ makeNode f r c n
fromLoc n (SpanPoint f r c)         = Just $ makeNode f r c n
fromLoc n _                         = Nothing


-- Get NodeInfo out of an annotated Python node
fromPyAnnotation :: (Pretty (n SrcSpan), P.Annotated n) => n SrcSpan -> Maybe NodeInfo
fromPyAnnotation n = fromLoc n (P.annot n)


-- Convert a Python expression into a generic expression
fromPyExpr :: P.Expr anno -> Expr
fromPyExpr (P.BinaryOp op e1 e2 _) = BinaryOp (fromPyOp op)
                                              (fromPyExpr e1)
                                              (fromPyExpr e2)
fromPyExpr e = UnkExpr (show $ pretty e)

fromPyExpr' :: P.Expr SrcSpan -> AExpr
fromPyExpr' n = let ni = fromPyAnnotation n
                    e  = fromPyExpr n
                in Annotated ni e


allPyExprs :: (Data a) => P.Module a -> [P.Expr a]
allPyExprs = listify (const True)

parse :: PythonVersion -> FilePath -> IO (Either String Ast)
parse ver f = do
  content <- openFile f ReadMode >>= hGetContents
  let parser = case ver of
                 Python2 -> P2.parseModule
                 Python3 -> P3.parseModule
  let result = parser content f
  case result of
    Left pe               -> return $ Left (show pe)
    Right (mod, comments) -> do
      let all = allPyExprs mod
      let exprs = map fromPyExpr' all
      return $ Right $ Ast exprs


