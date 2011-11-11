{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.Python where

import qualified Language.Python.Common.AST as P
import           Language.Python.Common.SrcLocation
import           Language.Python.Version2.Parser as P2
import           Language.Python.Version3.Parser as P3
import           Language.Python.Common.Pretty
import           Language.Python.Common.PrettyAST()
import           Semgrep.Languages.Generic
import           Control.Monad
import           Data.Generics
import           Data.Maybe(maybeToList)
import           System.IO
import           Semgrep ( PythonVersion(..)
                         )

type PyAnno = SrcSpan
type PyStmt     = P.Statement PyAnno
type PyAssignOp = P.AssignOp PyAnno
type PyExpr     = P.Expr PyAnno
type PyModule   = P.Module PyAnno

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
fromPyExpr n@(P.BinaryOp op e1 e2 _) = BinaryOp (fromPyOp op)
                                                (fromPyExpr e1)
                                                (fromPyExpr e2)
                                                (fromPyAnnotation n)

fromPyExpr n@(P.Var ident _) = Var (P.ident_string ident)
                                   (fromPyAnnotation n)

fromPyExpr n@(P.Int val lit _) = LiteralValue (IntLiteral $ fromInteger val)
                                              (Just lit)
                                              (fromPyAnnotation n)

fromPyExpr n@(P.Strings strs _) =
  let stringConsts   = map StringLiteral strs
      ann            = fromPyAnnotation n
      makeLiteralValue c = LiteralValue c Nothing Nothing
      compoundExpr   = CompoundExpr (map makeLiteralValue stringConsts) ann
  in case fromCompoundedStrings compoundExpr of
    Nothing -> compoundExpr
    Just x  -> x

fromPyExpr e = UnkExpr (show $ pretty e) Nothing


-- | Convert Python if/elif to generic if statement
fromPyElIf :: (PyExpr, [PyStmt]) -> Stmt
fromPyElIf (expr, stmts) =
  IfStmt (fromPyExpr expr)
         (toBlock stmts Nothing)
         Nothing
         Nothing


toBlock :: [PyStmt] -> Annotation -> Stmt
toBlock stmts = Block (map fromPyStmt stmts)


-- | Convert a list of python if/elif statements into a single generic IfStmt
fromPyIf :: Maybe PyStmt -> [(PyExpr, [PyStmt])] -> Maybe Stmt -> Maybe Stmt
-- If our if/elif list is empty, just return the else block (if it exists)
fromPyIf cond [] el     = el
fromPyIf cond (t:ts) el =

  -- Extract one if/elif expression and block statement
  let (IfStmt e cs _ _) = fromPyElIf t

  -- Build a new if or elif, recursively applying this function
  -- for further elif/else statements
  in Just $ IfStmt e cs (fromPyIf Nothing ts el)
                        (join $ fmap fromPyAnnotation cond)



-- | Convert a Python statement into a generic statement
fromPyStmt :: PyStmt -> Stmt
fromPyStmt n@(P.Fun name' args result body _) =
  let ann = fromPyAnnotation n
  in DeclStmt $ Function (maybeToList $ fmap (Result . fromPyExpr) result)
                         (Just $ P.ident_string name')
                         (toBlock body ann)
                         ann

-- | Conditional if/elif/el statements
fromPyStmt n@(P.Conditional ifs el a) =
  let maybeElse = case el of
                    [] -> Nothing
                    el -> Just $ toBlock el Nothing
  in case fromPyIf (Just n) ifs maybeElse of
       Nothing  -> error "empty conditional"
       Just iff -> iff

-- | Expression statements
fromPyStmt n@(P.StmtExpr expr _) = ExprStmt (Just $ fromPyExpr expr)
                                            (fromPyAnnotation n)
-- | Assignment statements
fromPyStmt n@(P.Assign exprs exprFrom _) =
  let ann       = fromPyAnnotation n
      exprFrom' = fromPyExpr exprFrom
  in case exprs of
    []  -> error "empty assign"
    [a] -> toSimpleExprStmt $ Assign DefaultAssign
                                     (fromPyExpr a)
                                     exprFrom'
                                     ann
    _   -> toSimpleExprStmt $ DestructuringAssign (map fromPyExpr exprs)
                                                  exprFrom'
                                                  ann
-- | Augmented Assignment statements (eg. +=, -=, etc)
fromPyStmt n@(P.AugmentedAssign e1 op e2 _) =
  toSimpleExprStmt $ Assign (fromPyAssignOp op)
                            (fromPyExpr e1)
                            (fromPyExpr e2)
                            (fromPyAnnotation n)

-- | Unknown Statement
fromPyStmt n = UnkStmt (show $ pretty n) (fromPyAnnotation n)

toSimpleExprStmt :: Expr -> Stmt
toSimpleExprStmt a = ExprStmt (Just a) Nothing

fromPyAssignOp :: PyAssignOp -> AssignOp
fromPyAssignOp (P.PlusAssign _)       = PlusAssign
fromPyAssignOp (P.DivAssign _)        = DivAssign
fromPyAssignOp (P.MultAssign _)       = MulAssign
fromPyAssignOp (P.PlusAssign _)       = PlusAssign
fromPyAssignOp (P.MinusAssign _)      = MinusAssign
fromPyAssignOp (P.ModAssign _)        = ModAssign
fromPyAssignOp (P.PowAssign _)        = PowAssign
fromPyAssignOp (P.BinAndAssign _)     = BinAndAssign
fromPyAssignOp (P.BinOrAssign _)      = BinOrAssign
fromPyAssignOp (P.BinXorAssign _)     = BinXorAssign
fromPyAssignOp (P.LeftShiftAssign _)  = LeftShiftAssign
fromPyAssignOp (P.RightShiftAssign _) = RightShiftAssign
fromPyAssignOp (P.FloorDivAssign _)   = FloorDivAssign
fromPyAssignOp n                    = UnkAssign (show n)


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


