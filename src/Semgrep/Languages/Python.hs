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
-- | Pretty print to maybe string
--------------------------------------------------------------------------------
justShowP :: (Pretty a) => a -> Maybe String
justShowP = Just . show . pretty

--------------------------------------------------------------------------------
-- | NInfo helper constructor
--------------------------------------------------------------------------------
makeNode :: (Pretty n) => String -> Int -> Int -> n -> NInfo
makeNode f r c n = NInfo (Just $ makePos f r c)
                         (Just . show . pretty $ n)

--------------------------------------------------------------------------------
-- | Build a NInfo given a pretty printable node and SrcLocation
--------------------------------------------------------------------------------
fromSpan :: (Pretty n) => n -> PyAnno -> NInfo
fromSpan n (SpanCoLinear f r c _)    = makeNode f r c n
fromSpan n (SpanMultiLine f r c _ _) = makeNode f r c n
fromSpan n (SpanPoint f r c)         = makeNode f r c n
fromSpan n _                         = NInfo Nothing (justShowP n)

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
fromPyIdent :: P.Ident PyAnno -> Identifier
fromPyIdent n@(P.Ident s _) = Ident s Nothing (fromPyInfo n)

--------------------------------------------------------------------------------
-- | Get NInfo out of an annotated Python node
--------------------------------------------------------------------------------
fromPyInfo :: (P.Annotated n, Pretty (n PyAnno))
           => n PyAnno
           -> NInfo
fromPyInfo n = fromSpan n (P.annot n)

--------------------------------------------------------------------------------
-- | Expressions
--------------------------------------------------------------------------------
fromPyExpr :: PyExpr -> Node
fromPyExpr n@(P.BinaryOp op e1 e2 _) = BinaryOp (fromPyOp op)
                                                (fromPyExpr e1)
                                                (fromPyExpr e2)
                                                (fromPyInfo n)
                                                Expression

--------------------------------------------------------------------------------
-- | Variables
--------------------------------------------------------------------------------
fromPyExpr n@(P.Var ident _) = Var (fromPyIdent ident)
                                   (fromPyInfo n)
                                   Expression

--------------------------------------------------------------------------------
-- | Int literals
--------------------------------------------------------------------------------
fromPyExpr n@(P.Int val lit _) = Literal (IntLiteral $ fromInteger val)
                                         (Just lit)
                                         (fromPyInfo n)
                                         Expression

--------------------------------------------------------------------------------
-- | String literals
--------------------------------------------------------------------------------
fromPyExpr n@(P.Strings strs _) =
  let stringConsts   = map StringLiteral strs
      ann            = fromPyInfo n
      makeLiteral c  = Literal c Nothing (NInfo Nothing Nothing) Expression
  in Compound (map makeLiteral stringConsts) ann Expression


--------------------------------------------------------------------------------
-- | Function application
--------------------------------------------------------------------------------
fromPyExpr n@(P.Call e args _) = Call (fromPyExpr e) []
                                      (fromPyInfo n)
                                      Expression


--------------------------------------------------------------------------------
-- | Unknown expressions
--------------------------------------------------------------------------------
fromPyExpr e = UnkNode "" (fromPyInfo e) Expression


--------------------------------------------------------------------------------
-- | Convert Python if/elif to generic if statement
--------------------------------------------------------------------------------
fromPyElIf :: (PyExpr, [PyStmt]) -> Node
fromPyElIf (expr, stmts) =
  If (fromPyExpr expr)
     (toBlock stmts nullInfo Statement)
     Nothing
     nullInfo
     Expression


--------------------------------------------------------------------------------
-- | Python 'Suite' to generic Block
--------------------------------------------------------------------------------
toBlock :: [PyStmt] -> NInfo -> NKind -> Node
toBlock stmts = Block (map fromPyStmt stmts)


--------------------------------------------------------------------------------
-- | Convert a list of python if/elif statements into a single generic If
--------------------------------------------------------------------------------
fromPyIf :: Maybe PyStmt -> [(PyExpr, [PyStmt])] -> Maybe Node -> Maybe Node
-- If our if/elif list is empty, just return the else block (if it exists)
fromPyIf _ [] el        = el
fromPyIf cond (t:ts) el =

  -- Extract one if/elif expression and block statement
  let (If e cs _ _ _) = fromPyElIf t

  -- Build a new if or elif, recursively applying this function
  -- for further elif/else statements
  in Just $ If e cs (fromPyIf Nothing ts el)
                    (case fmap fromPyInfo cond of
                      Nothing -> nullInfo
                      Just a  -> a)
                    Statement

--------------------------------------------------------------------------------
-- | Function declarations
--------------------------------------------------------------------------------
fromPyStmt :: PyStmt -> Node
fromPyStmt n@(P.Fun name' args result body _) =
  let ann = fromPyInfo n
  in Function (maybeToList $ fmap (Result . fromPyExpr) result)
              (Just $ fromPyIdent name')
              (toBlock body ann Statement)
              ann
              Statement

--------------------------------------------------------------------------------
-- | Conditional if/elif/el statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Conditional ifs el a) =
  let maybeElse = case el of
                    [] -> Nothing
                    el -> Just $ toBlock el nullInfo Statement
  in case fromPyIf (Just n) ifs maybeElse of
       Nothing  -> error "empty conditional"
       Just iff -> iff


--------------------------------------------------------------------------------
-- | Expression statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.StmtExpr expr _) = Singleton (fromPyExpr expr)
                                             (fromPyInfo n)
                                             Statement

--------------------------------------------------------------------------------
-- | Assignment statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Assign exprs exprFrom _) =
  let ann       = fromPyInfo n
      exprFrom' = fromPyExpr exprFrom
  in case exprs of
    []  -> error "empty assign"
    [a] -> Assign DefaultAssign (fromPyExpr a) exprFrom' ann Statement
    _   -> DestructuringAssign (map fromPyExpr exprs) exprFrom' ann Statement

--------------------------------------------------------------------------------
-- | Augmented assignment statements (eg. +=, -=, etc)
--------------------------------------------------------------------------------
fromPyStmt n@(P.AugmentedAssign e1 op e2 _) = Assign (fromPyAssignOp op)
                                                     (fromPyExpr e1)
                                                     (fromPyExpr e2)
                                                     (fromPyInfo n)
                                                     Statement

--------------------------------------------------------------------------------
-- | Class statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Class name args body _) = Class (fromPyIdent name)
                                                (map fromPyStmt body)
                                                (fromPyInfo n)
                                                Statement

--------------------------------------------------------------------------------
-- | Return statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Return mExpr _) = Return (fmap fromPyExpr mExpr)
                                         (fromPyInfo n)
                                         Statement

--------------------------------------------------------------------------------
-- | Import statements
--------------------------------------------------------------------------------
fromPyStmt n@(P.Import items _) = Import (map fromPyImportItem items)
                                         Nothing
                                         (fromPyInfo n)
                                         Statement


--------------------------------------------------------------------------------
-- | Unknown statements
--------------------------------------------------------------------------------
fromPyStmt n = UnkNode "" (fromPyInfo n) Statement

--------------------------------------------------------------------------------
-- | Convert a Python import item to a generic statement
--------------------------------------------------------------------------------
fromPyImportItem :: P.ImportItem PyAnno -> ImportItem
fromPyImportItem n@(P.ImportItem names maybeIdent _) =
  ImportItem (map fromPyIdent names)
             (fmap fromPyIdent maybeIdent)
             (fromPyInfo n)

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
fromPyModule a@(P.Module stmts) = Module (map fromPyStmt stmts)
                                         Nothing
                                         nullInfo


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


