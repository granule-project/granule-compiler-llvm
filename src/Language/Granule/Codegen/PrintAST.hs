module Language.Granule.Codegen.PrintAST where

import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Identifiers (Id (Id))
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type

-- converts an AST to JSON for debugging & visualisation. WIP

data JSON = Str String | Obj [(String, JSON)] | Arr [JSON]

printAST :: AST ev Type -> String
printAST ast = toString (jAST ast)

toString :: JSON -> String
toString (Str s) = "\"" ++ s ++ "\""
toString (Obj pairs) = "{" ++ commas (map field pairs) ++ "}"
toString (Arr elems) = "[" ++ commas (map toString elems) ++ "]"

field :: (String, JSON) -> String
field (key, value) = "\"" ++ key ++ "\":" ++ toString value

commas :: [String] -> String
commas [] = ""
commas [x] = x
commas (x : xs) = x ++ "," ++ commas xs

node :: String -> [(String, JSON)] -> JSON
node name fields = Obj [(name, Obj fields)]

jAST :: AST ev Type -> JSON
jAST (AST _ defs _ _ _) = Arr (map jDef defs)

jDef :: Def ev Type -> JSON
jDef (Def _ _ _ _ equations _) = jEqList equations

jEqList :: EquationList ev Type -> JSON
jEqList (EquationList _ _ _ equations) = Arr (map jEq equations)

jEq :: Equation ev Type -> JSON
jEq (Equation s i a b ps expr) = jExpr expr

jExpr :: Expr ev Type -> JSON
jExpr (App _ ty _ fn arg) = node "App" [("type", jTy ty), ("fn", jExpr fn), ("arg", jExpr arg)]
jExpr (Binop {}) = Str "%BINOP%"
jExpr (LetDiamond {}) = Str "%LETDIAMOND%"
jExpr (Val _ ty _ (Promote ty' expr)) = jExpr expr
jExpr (Val _ ty _ val) = node "Val" [("type", jTy ty), ("val", jVal val)]
jExpr (Case {}) = Str "%CASE%"
jExpr (Hole {}) = Str "%HOLE%"
jExpr (AppTy {}) = Str "%APP_TY%"
jExpr (TryCatch {}) = Str "%TRY_CATCH%"
jExpr (Unpack _ ty _ _ id e1 e2) = node "Unpack" [("type", jTy ty), ("id", jId id), ("e1", jExpr e1), ("e2", jExpr e2)]

jVal :: Value ev Type -> JSON
jVal (Var ty id) = node "Var" [("type", jTy ty), ("id", jId id)]
jVal (Abs ty pat mty expr) = node "Abs" [("type", jTy ty), ("pattern", jPat pat), ("expr", jExpr expr)]
jVal (Promote ty expr) = node "Promote" [("type", jTy ty), ("expr", jExpr expr)]
jVal (Pure ty expr) = node "Pure" [("type", jTy ty), ("expr", jExpr expr)]
jVal (Constr ty id vals) = node "Constr" [("type", jTy ty), ("id", jId id), ("vals", Arr (map jVal vals))]
jVal (NumInt n) = Str (show n)
jVal (NumFloat n) = Str (show n)
jVal (CharLiteral c) = Str (show c)
jVal (StringLiteral s) = Str (show s)
jVal (Ext {}) = Str "%EXT%"
jVal (Nec {}) = Str "%NEC%"
jVal (Pack {}) = Str "%PACK%"
jVal (TyAbs {}) = Str "%TY_ABS%"

jPat :: Pattern Type -> JSON
jPat (PVar _ ty _ id) = node "PVar" [("type", jTy ty), ("id", jId id)]
jPat (PWild {}) = Str "%PWILD%"
jPat (PBox _ ty _ p) = node "PBox" [("ty", jTy ty), ("pat", jPat p)]
jPat (PInt {}) = Str "%PINT%"
jPat (PFloat {}) = Str "%PFLOAT%"
jPat (PConstr _ ty _ id _ pats) = node "PConstr" [("type", jTy ty), ("id", jId id), ("pats", Arr (map jPat pats))]

jId :: Id -> JSON
jId id = Str (sId id)

jTy :: Type -> JSON
jTy ty = Str (sTy ty)

sId :: Id -> String
sId (Id _ id) = id

paren :: String -> String
paren str = "(" ++ str ++ ")"

named :: String -> String -> String
named name str = name ++ " " ++ paren str

sTy :: Type -> String
sTy (Type {}) = "type"
sTy (FunTy id _ arg ret) = sTy arg ++ " -> " ++ sTy ret
sTy (TyCon id) = sId id
sTy (Box _ ty) = "[" ++ sTy ty ++ "]"
sTy (Diamond {}) = "%DIAMOND%"
sTy (Star {}) = "%STAR%"
sTy (Borrow (TyCon (Id "Star" "Star")) ty) = "*" ++ paren (sTy ty)
sTy (Borrow p ty) = "& " ++ paren (sTy p) ++ " " ++ paren (sTy ty)
sTy (TyVar id) = "TyVar (" ++ sId id ++ ")"
sTy (TyApp t1 t2) = "(" ++ sTy t1 ++ ") (" ++ sTy t2 ++ ")"
sTy (TyInt {}) = "%TY_INT%"
sTy (TyRational {}) = "%TY_RATIONAL%"
sTy (TyFraction {}) = "%TY_FRACTION%"
sTy (TyGrade {}) = "%TY_GRADE%"
sTy (TyInfix {}) = "%TY_INFIX%"
sTy (TySet {}) = "%TY_SET%"
sTy (TyCase {}) = "%TY_CASE%"
sTy (TySig {}) = "%TY_SIG%"
-- sTy (TyExists id kind ty) = "exists {" ++ sId id ++ " : " ++ sTy kind ++ "} . " ++ sTy ty
sTy (TyExists id kind ty) = sTy ty
sTy (TyForall id kind ty) = "forall {" ++ sId id ++ " : " ++ sTy kind ++ "} . " ++ sTy ty
sTy (TyName {}) = "%TY_NAME%"
