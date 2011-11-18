{-# LANGUAGE ViewPatterns #-}

module Semgrep.Languages.C (
  parse
) where


import qualified Language.C as C
import qualified Language.C.Data.Ident as I
import           Language.C.System.GCC(newGCC)
import           Control.Monad
import           Semgrep.Languages.Generic
import           Data.Generics
import           Semgrep()


--------------------------------------------------------------------------------
-- | Assignment operators
--------------------------------------------------------------------------------
fromCAssignOp :: C.CAssignOp -> AssignOp
fromCAssignOp C.CAssignOp = DefaultAssign
fromCAssignOp C.CMulAssOp = MulAssign
fromCAssignOp C.CDivAssOp = DivAssign
fromCAssignOp o = UnkAssign (show o)


--------------------------------------------------------------------------------
-- | Binary operators
--------------------------------------------------------------------------------
fromCBinOp :: C.CBinaryOp -> BinOp
fromCBinOp C.CLeOp  = LeOp
fromCBinOp C.CGrOp  = GrOp
fromCBinOp C.CLeqOp = LeqOp
fromCBinOp C.CGeqOp = GeqOp
fromCBinOp C.CEqOp  = EqOp
fromCBinOp C.CNeqOp = NeqOp
fromCBinOp C.CAddOp = AddOp
fromCBinOp o        = UnkOp (gshow o)


--------------------------------------------------------------------------------
-- | Converts a C literal value to a generic literal
--------------------------------------------------------------------------------
fromCConst :: C.CConst -> LiteralValue
fromCConst (C.CIntConst cint _) = IntLiteral (fromInteger $ C.getCInteger cint)
fromCConst (C.CCharConst cchar _) = CharLiteral (head $ C.getCChar cchar)
fromCConst (C.CFloatConst (C.CFloat s) _) = FloatLiteral (read s)
fromCConst (C.CStrConst cstring _) = StringLiteral (C.getCString cstring)

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
fromCIdent :: I.Ident -> Identifier
fromCIdent (I.Ident s i _) = Ident s (Just i) Nothing

--------------------------------------------------------------------------------
-- | Converts a C expression to a generic expression
--------------------------------------------------------------------------------
fromCExpr :: C.CExpr -> Expr
fromCExpr n@(C.CVar ident _) = Var (fromCIdent ident)
                                   (toAnnotation n)

--------------------------------------------------------------------------------
-- | Assignment expressions
--------------------------------------------------------------------------------
fromCExpr n@(C.CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                            (fromCExpr e1)
                                            (fromCExpr e2)
                                            (toAnnotation n)

--------------------------------------------------------------------------------
-- | Literal values
--------------------------------------------------------------------------------
fromCExpr n@(C.CConst c) = LiteralValue (fromCConst c)
                                        Nothing
                                        (toAnnotation n)

--------------------------------------------------------------------------------
-- | Conditional operations (?:)
--------------------------------------------------------------------------------
fromCExpr n@(C.CCond e1 e2 e3 _) = ConditionalOp (fromCExpr e1)
                                                 (fmap fromCExpr e2)
                                                 (fromCExpr e3)
                                                 (toAnnotation n)

--------------------------------------------------------------------------------
-- | Binary operations
--------------------------------------------------------------------------------
fromCExpr n@(C.CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                              (fromCExpr e1)
                                              (fromCExpr e2)
                                              (toAnnotation n)

--------------------------------------------------------------------------------
-- | Function application
--------------------------------------------------------------------------------
fromCExpr n@(C.CCall expr exprs _) = Call (fromCExpr expr)
                                          (map fromCExpr exprs)
                                          (toAnnotation n)

--------------------------------------------------------------------------------
-- | Unknown expressions
--------------------------------------------------------------------------------
fromCExpr e = UnkExpr (gshow e) (toAnnotation e)

--------------------------------------------------------------------------------
-- | Takes a CNode and returns a generic NodeInfo
--------------------------------------------------------------------------------
toAnnotation :: (C.Pretty a, C.Pos a) => a -> Maybe NodeInfo
toAnnotation p = Just $ NodeInfo (makePos p) (makePretty p)
  where
    makePretty   = Just . show . C.pretty
    makePos      = Just . makePos' . C.posOf
    makePos' pos = Position Nothing
                            (Just $ C.posFile pos)
                            (Just $ C.posRow pos)
                            (Just $ C.posColumn pos)


--------------------------------------------------------------------------------
-- | Takes a C statement a returns if it's an 'If' statement or not
--------------------------------------------------------------------------------
isIfStmt :: C.CStat -> Bool
isIfStmt (C.CIf _ _ _ _) = True
isIfStmt _ = False


--------------------------------------------------------------------------------
-- | Traverses a C translation unit and returns a list of C if statements
--------------------------------------------------------------------------------
ifStmts :: C.CTranslUnit -> [C.CStat]
ifStmts = listify isIfStmt


allCExprs :: C.CTranslUnit -> [C.CExpr]
allCExprs = listify (const True)


fromCDecl :: C.CDecl -> Decl
fromCDecl (C.CDecl {}) = UnkDecl "not implemented" Nothing


fromCCompoundBlock :: C.CBlockItem -> Either Stmt Decl
fromCCompoundBlock (C.CBlockStmt stmt) = Left $ fromCStmt stmt
fromCCompoundBlock (C.CBlockDecl decl) = Right $ fromCDecl decl
fromCCompoundBlock (C.CNestedFunDef funDef) = Right $ fromCFunctionDef funDef


--------------------------------------------------------------------------------
-- | Convert a C statement into a generic statement
--------------------------------------------------------------------------------
fromCStmt :: C.CStat -> Stmt
fromCStmt n@(C.CLabel ident stmt attrs _) = Label (fromCIdent ident)
                                                  (fromCStmt stmt)
                                                  (toAnnotation n)

--------------------------------------------------------------------------------
-- | Return statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CReturn mExpr _) = Return (fmap fromCExpr mExpr)
                                         (toAnnotation n)

--------------------------------------------------------------------------------
-- | Case statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CCase e1 s1 _) = CaseStmt (fromCExpr e1)
                                         (fromCStmt s1)
                                         (toAnnotation n)

--------------------------------------------------------------------------------
-- | Case statement default case
--------------------------------------------------------------------------------
fromCStmt n@(C.CDefault s1 _) = CaseStmtDefault (fromCStmt s1) (toAnnotation n)

--------------------------------------------------------------------------------
-- | Expression statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CExpr mExpr _) = ExprStmt (fmap fromCExpr mExpr) (toAnnotation n)

--------------------------------------------------------------------------------
-- | If statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CIf e1 s1 ms1 _) = IfStmt (fromCExpr e1)
                                         (fromCStmt s1)
                                         (fmap fromCStmt ms1)
                                         (toAnnotation n)

--------------------------------------------------------------------------------
-- | Switch statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CSwitch e1 s1 _) = SwitchStmt (fromCExpr e1)
                                             (fromCStmt s1)
                                             (toAnnotation n)

--------------------------------------------------------------------------------
-- | Convert 'Compound' statements to a generic Block
--------------------------------------------------------------------------------
fromCStmt n@(C.CCompound localLabels blockItems _) =
  let converted = map (stmtOrDecl . fromCCompoundBlock) blockItems
  in Block converted (toAnnotation n)
    where
      stmtOrDecl (Left stmt)  = stmt
      stmtOrDecl (Right decl) = DeclStmt decl

--------------------------------------------------------------------------------
-- | Unknoown statements
--------------------------------------------------------------------------------
fromCStmt n = UnkStmt (gshow n) (toAnnotation n)


--------------------------------------------------------------------------------
-- | Function declarations
--------------------------------------------------------------------------------
fromCFunctionDef :: C.CFunDef -> Decl
fromCFunctionDef n@(C.CFunDef declSpecs d1 d2 stmt _) =
  let name  = Nothing
      stmt' = fromCStmt stmt
      node  = toAnnotation n
  in Function [] name stmt' node

--------------------------------------------------------------------------------
-- | CExtDecl to Decl
--------------------------------------------------------------------------------
fromCExtDecl :: C.CExtDecl -> Decl
fromCExtDecl (C.CFDefExt d) = fromCFunctionDef d
fromCExtDecl (C.CDeclExt d) = fromCDecl d
fromCExtDecl n = UnkDecl (gshow n) (toAnnotation n)


--------------------------------------------------------------------------------
-- | Build a module from a C translation unit
--------------------------------------------------------------------------------
fromCTranslUnit :: C.CTranslUnit -> Module
fromCTranslUnit n@(C.CTranslUnit extDecls _) =
  let ann   = toAnnotation n
      fromD = DeclStmt . fromCExtDecl
  in Module (map fromD extDecls)
            (translUnitModuleName n)
            ann

translUnitModuleName :: C.CTranslUnit -> Maybe String
translUnitModuleName =
  return . fileNameToModuleName <=< posFilename <=< posOf . toAnnotation

fileNameToModuleName :: String -> String
fileNameToModuleName = takeWhile (/= '.')

--------------------------------------------------------------------------------
-- | Takes a FilePath string and returns a generic AST, annotated with NodeInfo
--   from the C expressions
--------------------------------------------------------------------------------
parse :: FilePath -> [String] -> IO (Either String Project)
parse file incs = do
  let gcc = newGCC "gcc"
  parsedC <- C.parseCFile gcc Nothing incs file

  case parsedC of
    Left txt        -> return $ Left $ show txt
    Right transUnit -> return $ Right $ Project [fromCTranslUnit transUnit]

