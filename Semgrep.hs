{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable, DeriveFunctor #-}


import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Text.PrettyPrint.HughesPJ
import qualified Language.C as C
import           Language.C.Data.Ident
import           Language.C.Analysis.AstAnalysis
import qualified Language.Python.Version2 as P
import           Data.Generics
import           Debug.Trace
import           System.Environment

data Annotated a b = Annotated a b
                   deriving (Show, Typeable, Data, Functor)

type AExpr b = Annotated Expr (NodeInfo b)

data Position = Position
              { posOffset :: Maybe Int
              , posFilename :: Maybe String
              , posLineNumber :: Maybe Int
              , posColumnNumber :: Maybe Int
              } deriving (Show)

data NodeInfo orig = NodeInfo Position orig

data BinOp = LeOp
           | GrOp
           | LeqOp
           | GeqOp
           | EqOp
           | NeqOp
           | AddOp
           | UnkOp String
           deriving (Show, Typeable, Data, Eq)

data AssignOp = AssignOp
              | MulAssignOp
              | DivAssignOp
              | UnkAssignOp String
              deriving (Show, Typeable, Data, Eq)

data ConstVal = IntConst Int
              | CharConst Char
              | FloatConst Float
              | StringConst String
           deriving (Show, Typeable, Data, Eq)

data Expr = Var String
          | ConstVal ConstVal
          | BinaryOp BinOp Expr Expr
          | Assign AssignOp Expr Expr
          | ConditionalOp Expr (Maybe Expr) Expr
          | UnkExpr String
          deriving (Show, Typeable, Data)

instance C.Pretty a => Show (NodeInfo a) where
  show (NodeInfo p a) =
    "NodeInfo " ++ show p ++ " \"" ++ (show $ C.pretty a) ++ "\""


gtrace a = trace (gshow a) a

fromCAssignOp :: C.CAssignOp -> AssignOp
fromCAssignOp C.CAssignOp = AssignOp
fromCAssignOp C.CMulAssOp = MulAssignOp
fromCAssignOp C.CDivAssOp = DivAssignOp
fromCAssignOp o = UnkAssignOp (show o)


fromCBinOp :: C.CBinaryOp -> BinOp
fromCBinOp C.CLeOp  = LeOp
fromCBinOp C.CGrOp  = GrOp
fromCBinOp C.CLeqOp = LeqOp
fromCBinOp C.CGeqOp = GeqOp
fromCBinOp C.CEqOp  = EqOp
fromCBinOp C.CNeqOp = NeqOp
fromCBinOp C.CAddOp = AddOp
fromCBinOp o        = UnkOp (gshow o)


fromCConst :: C.CConst -> ConstVal
fromCConst (C.CIntConst cint _)         = IntConst (fromInteger $ C.getCInteger cint)
fromCConst (C.CCharConst cchar _)       = CharConst (head $ C.getCChar cchar)
fromCConst (C.CFloatConst (C.CFloat s) _) = FloatConst (read s)
fromCConst (C.CStrConst cstring _)      = StringConst (C.getCString cstring)


fromCExpr :: C.CExpr -> Expr
fromCExpr (C.CVar ident _)       = Var (identToString ident)
fromCExpr (C.CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                          (fromCExpr e1)
                                          (fromCExpr e2)
fromCExpr (C.CConst c)           = ConstVal (fromCConst c)
fromCExpr (C.CCond e1 e2 e3 _)   = ConditionalOp (fromCExpr e1)
                                                 (fmap fromCExpr e2)
                                                 (fromCExpr e3)
fromCExpr (C.CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                            (fromCExpr e1)
                                            (fromCExpr e2)
fromCExpr e = UnkExpr (show e)

cNodeInfo :: (C.CNode a) => a -> NodeInfo a
cNodeInfo n = NodeInfo (nodeInfo $ C.nodeInfo n) n
  where
    nodeInfo (C.OnlyPos p len)         = makePos p Nothing
    nodeInfo (C.NodeInfo p len name) = makePos p Nothing
    makePos pos mn = Position (Just $ C.posOffset pos)
                              (mn <|> (Just $ C.posFile pos))
                              (Just $ C.posRow pos)
                              (Just $ C.posColumn pos)

annotCNode :: C.CExpr -> Expr -> AExpr C.CExpr
annotCNode cexpr expr = Annotated expr (cNodeInfo cexpr)

fromCExpr' :: C.CExpr -> AExpr C.CExpr
fromCExpr' ce = annotCNode ce (fromCExpr ce)

isIfExpr :: C.CStat -> Bool
isIfExpr (C.CIf _ _ _ _) = True
isIfExpr _ = False


isCondOp :: BinOp -> Bool
isCondOp LeOp   = True
isCondOp GrOp   = True
isCondOp LeqOp  = True
isCondOp GeqOp  = True
isCondOp EqOp   = True
isCondOp NeqOp  = True
isCondOp _      = False


isCondExpr :: Expr -> Bool
isCondExpr (ConditionalOp op _ _ ) = True
isCondExpr (BinaryOp op _ _ )      = isCondOp op
isCondExpr _                       = False



ifStmts :: C.CTranslUnit -> [C.CStat]
ifStmts = listify isIfExpr


conditions :: [AExpr a] -> [AExpr a]
conditions = filter (\(Annotated expr _) -> isCondExpr expr)


process file = do
  putStrLn $ "file: " ++ file
  stream <- C.readInputStream file
  let parsedC = C.parseC stream (C.initPos file)

  case parsedC of
    Left txt -> print txt
    Right transUnit -> do
      let all   = allCExprs transUnit
      let convertedExprs = map fromCExpr' all
      let conds = conditions convertedExprs
      let ifs   = ifStmts transUnit

      putStrLn "\nAll Expressions"
      mapM_ prettyPrint all
      putStrLn "\nConditional Expressions"
      mapM_ print conds
      putStrLn "\nIf Statements"
      mapM_ prettyPrint ifs

  where
    prettyPrint = print . C.pretty

    allCExprs :: C.CTranslUnit -> [C.CExpr]
    allCExprs = listify (const True)

main = do
  [file] <- getArgs
  process file
