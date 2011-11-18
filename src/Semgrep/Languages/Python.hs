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

--------------------------------------------------------------------------------
-- | Binary operators
--------------------------------------------------------------------------------
fromPyOp :: (Data anno) => P.Op anno -> BinOp
fromPyOp (P.LessThan _)    = LeOp
fromPyOp (P.GreaterThan _) = GrOp
fromPyOp (P.Equality _)    = EqOp
fromPyOp o                 = UnkOp (gshow o)


--------------------------------------------------------------------------------
-- | Position helper constructor
--------------------------------------------------------------------------------
makePos :: String -> Int -> Int -> Position
makePos f r c = Position Nothing (Just f) (Just r) (Just c)


--------------------------------------------------------------------------------
-- | NodeInfo helper constructor
--------------------------------------------------------------------------------
makeNode :: (Pretty n) => String -> Int -> Int -> n -> NodeInfo
makeNode f r c n = NodeInfo (Just $ makePos f r c) (Just $ show $ pretty n)


--------------------------------------------------------------------------------
-- | Build a NodeInfo given a pretty printable node and SrcLocation
--------------------------------------------------------------------------------
fromSpan :: (Pretty n) => n -> PyAnno -> Maybe NodeInfo
fromSpan n (SpanCoLinear f r c _)    = Just $ makeNode f r c n
fromSpan n (SpanMultiLine f r c _ _) = Just $ makeNode f r c n
fromSpan n (SpanPoint f r c)         = Just $ makeNode f r c n
fromSpan _ _                         = Nothing

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
fromPyIdent :: P.Ident PyAnno -> Identifier
fromPyIdent n@(P.Ident s _) = Ident s Nothing (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Get NodeInfo out of an annotated Python node
--------------------------------------------------------------------------------
fromPyAnnotation :: (P.Annotated n, Pretty (n PyAnno))
                 => n PyAnno
                 -> Maybe NodeInfo
fromPyAnnotation n = fromSpan n (P.annot n)


--------------------------------------------------------------------------------
-- | Expressions
--------------------------------------------------------------------------------
fromPyExpr :: PyExpr -> Expr
fromPyExpr n@(P.BinaryOp op e1 e2 _) = BinaryOp (fromPyOp op)
                                                (fromPyExpr e1)
                                                (fromPyExpr e2)
                                                (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Variables
--------------------------------------------------------------------------------
fromPyExpr n@(P.Var ident _) = Var (fromPyIdent ident)
                                   (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Int literals
--------------------------------------------------------------------------------
fromPyExpr n@(P.Int val lit _) = LiteralValue (IntLiteral $ fromInteger val)
                                              (Just lit)
                                              (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | String literals
--------------------------------------------------------------------------------
fromPyExpr n@(P.Strings strs _) =
  let stringConsts   = map StringLiteral strs
      ann            = fromPyAnnotation n
      makeLiteralValue c = LiteralValue c Nothing Nothing
      compoundExpr   = CompoundExpr (map makeLiteralValue stringConsts) ann
  in case fromCompoundedStrings compoundExpr of
    Nothing -> compoundExpr
    Just x  -> x

--------------------------------------------------------------------------------
-- | Function application
--------------------------------------------------------------------------------
fromPyExpr n@(P.Call e args _) = Call (fromPyExpr e)
                                      []
                                      (fromPyAnnotation n)


--------------------------------------------------------------------------------
-- | Unknown expressions
--------------------------------------------------------------------------------
fromPyExpr e = UnkExpr (show $ pretty e) Nothing


--------------------------------------------------------------------------------
-- | Convert Python if/elif to generic if statement
--------------------------------------------------------------------------------
fromPyElIf :: (PyExpr, [PyStmt]) -> Stmt
fromPyElIf (expr, stmts) =
  IfStmt (fromPyExpr expr)
         (toBlock stmts Nothing)
         Nothing
         Nothing


--------------------------------------------------------------------------------
-- | Python 'Suite' to generic Block
--------------------------------------------------------------------------------
toBlock :: [PyStmt] -> Annotation -> Stmt
toBlock stmts = Block (map fromPyStmt stmts)


--------------------------------------------------------------------------------
-- | Convert a list of python if/elif statements into a single generic IfStmt
--------------------------------------------------------------------------------
fromPyIf :: Maybe PyStmt -> [(PyExpr, [PyStmt])] -> Maybe Stmt -> Maybe Stmt
-- If our if/elif list is empty, just return the else block (if it exists)
fromPyIf _ [] el        = el
fromPyIf cond (t:ts) el =

  -- Extract one if/elif expression and block statement
  let (IfStmt e cs _ _) = fromPyElIf t

  -- Build a new if or elif, recursively applying this function
  -- for further elif/else statements
  in Just $ IfStmt e cs (fromPyIf Nothing ts el)
                        (join $ fmap fromPyAnnotation cond)


--------------------------------------------------------------------------------
-- | Function declarations
--------------------------------------------------------------------------------
fromPyStmt :: PyStmt -> Stmt
fromPyStmt n@(P.Fun name' args result body _) =
  let ann = fromPyAnnotation n
  in DeclStmt $ Function (maybeToList $ fmap (Result . fromPyExpr) result)
                         (Just $ fromPyIdent name')
                         (toBlock body ann)
                         ann

--------------------------------------------------------------------------------
-- | Conditional if/elif/el statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Conditional ifs el a) =
  let maybeElse = case el of
                    [] -> Nothing
                    el -> Just $ toBlock el Nothing
  in case fromPyIf (Just n) ifs maybeElse of
       Nothing  -> error "empty conditional"
       Just iff -> iff


--------------------------------------------------------------------------------
-- | Expression statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.StmtExpr expr _) = ExprStmt (Just $ fromPyExpr expr)
                                            (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Assignment statements
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- | Augmented assignment statements (eg. +=, -=, etc)
--------------------------------------------------------------------------------
fromPyStmt n@(P.AugmentedAssign e1 op e2 _) =
  toSimpleExprStmt $ Assign (fromPyAssignOp op)
                            (fromPyExpr e1)
                            (fromPyExpr e2)
                            (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Class statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Class name args body _) =
  DeclStmt $ Class (fromPyIdent name)
                   (map fromPyStmt body)
                   (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Return statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Return mExpr _) = Return (fmap fromPyExpr mExpr)
                                         (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Import statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Import items _) = Import (map fromPyImportItem items)
                                         Nothing
                                         (fromPyAnnotation n)


--------------------------------------------------------------------------------
-- | Unknown statements
--------------------------------------------------------------------------------
fromPyStmt n = UnkStmt (show $ pretty n) (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Simple expression statements
--------------------------------------------------------------------------------
toSimpleExprStmt :: Expr -> Stmt
toSimpleExprStmt a = ExprStmt (Just a) Nothing

--------------------------------------------------------------------------------
-- | Convert a Python import item to a generic statement
--------------------------------------------------------------------------------
fromPyImportItem :: P.ImportItem PyAnno -> ImportItem
fromPyImportItem n@(P.ImportItem names maybeIdent _) =
  ImportItem (map fromPyIdent names)
             (fmap fromPyIdent maybeIdent)
             (fromPyAnnotation n)

--------------------------------------------------------------------------------
-- | Python 'augmented' assignment operators
--------------------------------------------------------------------------------
fromPyAssignOp :: PyAssignOp -> AssignOp
fromPyAssignOp (P.PlusAssign _)       = PlusAssign
fromPyAssignOp (P.DivAssign _)        = DivAssign
fromPyAssignOp (P.MultAssign _)       = MulAssign
fromPyAssignOp (P.MinusAssign _)      = MinusAssign
fromPyAssignOp (P.ModAssign _)        = ModAssign
fromPyAssignOp (P.PowAssign _)        = PowAssign
fromPyAssignOp (P.BinAndAssign _)     = BinAndAssign
fromPyAssignOp (P.BinOrAssign _)      = BinOrAssign
fromPyAssignOp (P.BinXorAssign _)     = BinXorAssign
fromPyAssignOp (P.LeftShiftAssign _)  = LeftShiftAssign
fromPyAssignOp (P.RightShiftAssign _) = RightShiftAssign
fromPyAssignOp (P.FloorDivAssign _)   = FloorDivAssign
fromPyAssignOp n                      = UnkAssign (show n)

--------------------------------------------------------------------------------
-- | Convert a Python module to a generic module
--------------------------------------------------------------------------------
fromPyModule :: PyModule -> Module
fromPyModule a@(P.Module stmts) = Module (map (Stmt . fromPyStmt) stmts)
                                         Nothing
                                         Nothing


--------------------------------------------------------------------------------
-- | Get a list of all Python expressions from a Python module
--------------------------------------------------------------------------------
allPyExprs :: (Data a) => P.Module a -> [P.Expr a]
allPyExprs = listify (const True)


--------------------------------------------------------------------------------
-- | Parse a Python AST to a generic AST given the python version and
--   file path
--------------------------------------------------------------------------------
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


