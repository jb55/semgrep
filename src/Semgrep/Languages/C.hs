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
fromCConst :: C.CConst -> Literal
fromCConst (C.CIntConst cint _) = IntLiteral (fromInteger $ C.getCInteger cint)
fromCConst (C.CCharConst cchar _) = CharLiteral (head $ C.getCChar cchar)
fromCConst (C.CFloatConst (C.CFloat s) _) = FloatLiteral (read s)
fromCConst (C.CStrConst cstring _) = StringLiteral (C.getCString cstring)

--------------------------------------------------------------------------------
-- | Identifiers
--------------------------------------------------------------------------------
fromCIdent :: I.Ident -> Identifier
fromCIdent (I.Ident s i _) = Ident s (Just i) nullInfo

--------------------------------------------------------------------------------
-- | Converts a C expression to a generic expression
--------------------------------------------------------------------------------
fromCExpr :: C.CExpr -> Node
fromCExpr n@(C.CVar ident _) = Var (fromCIdent ident)
                                   (toAnnotation n)
                                   Expression

--------------------------------------------------------------------------------
-- | Assignment expressions
--------------------------------------------------------------------------------
fromCExpr n@(C.CAssign op e1 e2 _) = Assign (fromCAssignOp op)
                                            (fromCExpr e1)
                                            (fromCExpr e2)
                                            (toAnnotation n)
                                            Expression

--------------------------------------------------------------------------------
-- | Literal values
--------------------------------------------------------------------------------
fromCExpr n@(C.CConst c) = Literal (fromCConst c)
                                   Nothing
                                   (toAnnotation n)
                                   Expression

--------------------------------------------------------------------------------
-- | Conditional operations (?:)
--------------------------------------------------------------------------------
fromCExpr n@(C.CCond e1 e2 e3 _) = ConditionalOp (fromCExpr e1)
                                                 (fmap fromCExpr e2)
                                                 (fromCExpr e3)
                                                 (toAnnotation n)
                                                 Expression

--------------------------------------------------------------------------------
-- | Binary operations
--------------------------------------------------------------------------------
fromCExpr n@(C.CBinary op e1 e2 _) = BinaryOp (fromCBinOp op)
                                              (fromCExpr e1)
                                              (fromCExpr e2)
                                              (toAnnotation n)
                                              Expression

--------------------------------------------------------------------------------
-- | Function application
--------------------------------------------------------------------------------
fromCExpr n@(C.CCall expr exprs _) = Call (fromCExpr expr)
                                          (map fromCExpr exprs)
                                          (toAnnotation n)
                                          Expression

--------------------------------------------------------------------------------
-- | Unknown expressions
--------------------------------------------------------------------------------
fromCExpr e = UnkNode (gshow e) (toAnnotation e) Expression

--------------------------------------------------------------------------------
-- | Takes a CNode and returns a generic NodeInfo
--------------------------------------------------------------------------------
toAnnotation :: (C.Pretty a, C.Pos a) => a -> NInfo
toAnnotation p = NInfo (makePos' p) (makePretty p)
  where
    makePretty    = Just . show . C.pretty
    makePos'      = Just . makePos'' . C.posOf
    makePos'' pos = Position Nothing
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


fromCDecl :: C.CDecl -> Node
fromCDecl (C.CDecl {}) = UnkNode "not implemented" nullInfo Declaration


fromCCompoundBlock :: C.CBlockItem -> Node
fromCCompoundBlock (C.CBlockStmt stmt) = fromCStmt stmt
fromCCompoundBlock (C.CBlockDecl decl) = fromCDecl decl
fromCCompoundBlock (C.CNestedFunDef funDef) = fromCFunctionDef funDef


--------------------------------------------------------------------------------
-- | Convert a C statement into a generic statement
--------------------------------------------------------------------------------
fromCStmt :: C.CStat -> Node
fromCStmt n@(C.CLabel ident stmt attrs _) = Label (fromCIdent ident)
                                                  (fromCStmt stmt)
                                                  (toAnnotation n)
                                                  Statement

--------------------------------------------------------------------------------
-- | Return statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CReturn mExpr _) = Return (fmap fromCExpr mExpr)
                                         (toAnnotation n)
                                         Statement

--------------------------------------------------------------------------------
-- | Case statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CCase e1 s1 _) = Case (Just $ fromCExpr e1)
                                     Match
                                     (fromCStmt s1)
                                     (toAnnotation n)
                                     Statement

--------------------------------------------------------------------------------
-- | Case statement default case
--------------------------------------------------------------------------------
fromCStmt n@(C.CDefault s1 _) = Case Nothing
                                     Default
                                     (fromCStmt s1)
                                     (toAnnotation n)
                                     Statement

--------------------------------------------------------------------------------
-- | Expression statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CExpr mExpr   _) =
  let ann  = toAnnotation n
      expr = case mExpr of
               Nothing -> UnkNode "empty expression" ann Statement
               Just e  -> fromCExpr e
  in Singleton expr ann Statement

--------------------------------------------------------------------------------
-- | If statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CIf e1 s1 ms1 _) = If (fromCExpr e1)
                                     (fromCStmt s1)
                                     (fmap fromCStmt ms1)
                                     (toAnnotation n)
                                     Statement

--------------------------------------------------------------------------------
-- | Switch statements
--------------------------------------------------------------------------------
fromCStmt n@(C.CSwitch e1 s1 _) = Switch (fromCExpr e1)
                                         (fromCStmt s1)
                                         (toAnnotation n)
                                         Statement

--------------------------------------------------------------------------------
-- | Convert 'Compound' statements to a generic Block
--------------------------------------------------------------------------------
fromCStmt n@(C.CCompound localLabels blockItems _) =
  let converted = map fromCCompoundBlock blockItems
  in Block converted (toAnnotation n) Statement

--------------------------------------------------------------------------------
-- | Unknown statements
--------------------------------------------------------------------------------
fromCStmt n = UnkNode (gshow n) (toAnnotation n) Statement


--------------------------------------------------------------------------------
-- | Function declarations
--------------------------------------------------------------------------------
fromCFunctionDef :: C.CFunDef -> Node
fromCFunctionDef n@(C.CFunDef declSpecs d1 d2 stmt _) =
  let name  = Nothing
      stmt' = fromCStmt stmt
      node  = toAnnotation n
  in Function [] name stmt' node Declaration

--------------------------------------------------------------------------------
-- | CExtDecl to Decl
--------------------------------------------------------------------------------
fromCExtDecl :: C.CExtDecl -> Node
fromCExtDecl (C.CFDefExt d) = fromCFunctionDef d
fromCExtDecl (C.CDeclExt d) = fromCDecl d
fromCExtDecl n = UnkNode (gshow n) (toAnnotation n) Declaration


--------------------------------------------------------------------------------
-- | Build a module from a C translation unit
--------------------------------------------------------------------------------
fromCTranslUnit :: C.CTranslUnit -> Module
fromCTranslUnit n@(C.CTranslUnit extDecls _) =
  let ann = toAnnotation n
  in Module (map fromCExtDecl extDecls)
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

