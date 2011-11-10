{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Python where

import qualified Language.Python.Common.AST as P
import           Language.Python.Common.SrcLocation
import           Language.Python.Version2.Parser as P2
import           Language.Python.Version3.Parser as P3
import           Language.Python.Common.Pretty
import           Language.Python.Common.PrettyAST
import           Semgrep.Languages.Generic
import           Control.Monad
import           Data.Generics
import           Data.Maybe
import           System.IO
import           Semgrep ( PythonVersion(..)
                         )

type PyAnno = SrcSpan
type PyStmt = P.Statement PyAnno
type PyExpr = P.Expr PyAnno
type PyModule = P.Module PyAnno

fromPyOp :: P.Op anno -> BinOp
fromPyOp (P.LessThan _)    = LeOp
fromPyOp (P.GreaterThan _) = GrOp
fromPyOp (P.Equality _)    = EqOp
fromPyOp o                 = UnkOp "dunno"

makePos :: String -> Int -> Int -> Position
makePos f r c = Position Nothing (Just f) (Just r) (Just c)

makeNode :: (Pretty n) => String -> Int -> Int -> n -> NodeInfo
makeNode f r c n = NodeInfo (Just $ makePos f r c) (Just $ show $ pretty n)

-- | Build a NodeInfo given a pretty printable node and SrcLocation
fromSpan :: (Pretty n) => n -> PyAnno -> Maybe NodeInfo
fromSpan n (SpanCoLinear f r c _)    = Just $ makeNode f r c n
fromSpan n (SpanMultiLine f r c _ _) = Just $ makeNode f r c n
fromSpan n (SpanPoint f r c)         = Just $ makeNode f r c n
fromSpan n _                         = Nothing


-- | Get NodeInfo out of an annotated Python node
fromPyAnnotation :: (P.Annotated n, Pretty (n PyAnno))
                 => n PyAnno
                 -> Maybe NodeInfo
fromPyAnnotation n = fromSpan n (P.annot n)


-- | Convert a Python expression into a generic expression
fromPyExpr :: PyExpr -> Expr
fromPyExpr o@(P.BinaryOp op e1 e2 _) = BinaryOp (fromPyOp op)
                                                (fromPyExpr e1)
                                                (fromPyExpr e2)
                                                (fromPyAnnotation o)
fromPyExpr e = UnkExpr (show $ pretty e) Nothing


-- | Convert Python if/elif to generic if statement
fromPyIfGuard :: (PyExpr, [PyStmt]) -> Stmt
fromPyIfGuard (expr, stmts) =
  IfStmt (fromPyExpr expr)
         (Block (map fromPyStmt stmts) Nothing)
         Nothing
         Nothing

fromPyIf :: Maybe PyStmt -> [(PyExpr, [PyStmt])] -> Maybe Stmt -> Maybe Stmt
fromPyIf cond [] el     = el
fromPyIf cond (t:ts) el =
  let (IfStmt e cs _ _) = fromPyIfGuard t
  in Just $ IfStmt e cs (fromPyIf Nothing ts el)
                        (join $ fmap fromPyAnnotation cond)

-- | Convert a Python statement into a generic statement
fromPyStmt :: PyStmt -> Stmt
fromPyStmt n@(P.Fun name args result body a) =
  let ann = fromPyAnnotation n
  in DeclStmt $ Function (Just . P.ident_string $ name)
                         (Block (map fromPyStmt body) ann)
                         ann

fromPyStmt n@(P.Conditional guards el a) =
  let maybeElse = case el of
                    [] -> Nothing
                    el -> Just $ Block (map fromPyStmt el) Nothing
  in case fromPyIf (Just n) guards maybeElse of
       Nothing  -> error "empty conditional"
       Just iff -> iff

fromPyStmt n = UnkStmt (show $ pretty n) (fromPyAnnotation n)

-- | Convert a Python module to a generic module
fromPyModule :: PyModule -> Module
fromPyModule a@(P.Module stmts) = Module (map fromPyStmt stmts) Nothing

allPyExprs :: (Data a) => P.Module a -> [P.Expr a]
allPyExprs = listify (const True)

parse :: PythonVersion -> FilePath -> IO (Either String Project)
parse ver f = do
  content <- openFile f ReadMode >>= hGetContents
  let parser = case ver of
                 Python2 -> P2.parseModule
                 Python3 -> P3.parseModule
  let result = parser content f
  case result of
    Left pe               -> return $ Left (show pe)
    Right (mod, comments) -> return $ Right $ Project [fromPyModule mod]


