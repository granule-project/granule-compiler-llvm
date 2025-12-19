module Language.Granule.Codegen.RewriteAST where

import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type
import Data.Bifunctor (second)

-- Rewrite Unpack ASTs into App Abs ASTs which our
-- compiler already knows how to handle.
-- TODO: handle unpack in compile

rewriteAST :: AST ev Type -> AST ev Type
rewriteAST ast = ast {definitions = map rewriteDef (definitions ast)}
  where
    rewriteDef def = def {defEquations = rewriteEquationList (defEquations def)}
    rewriteEquationList eqs = eqs {equations = map rewriteEquation (equations eqs)}
    rewriteEquation eq = eq {equationBody = rewriteExpr (equationBody eq)}

rewriteExpr :: Expr ev Type -> Expr ev Type
rewriteExpr expr =
  case expr of
    Unpack s ty r tyVar var e1 e2 ->
      let e1' = rewriteExpr e1
          e2' = rewriteExpr e2
          e1Ty = exprTy e1'
          absTy = FunTy Nothing Nothing e1Ty ty
          abs = Abs absTy (PVar s e1Ty r var) Nothing e2'
       in App s ty r (Val s absTy r abs) e1'
    App s retTy r f arg ->
      App s retTy r (rewriteExpr f) (rewriteExpr arg)
    Val s ty r val ->
      Val s ty r (rewriteVal val)
    Binop s ty r op e1 e2 ->
      Binop s ty r op (rewriteExpr e1) (rewriteExpr e2)
    Case s ty r e ps ->
      Case s ty r (rewriteExpr e) (map (second rewriteExpr) ps)
    AppTy s ty r e t ->
      AppTy s ty r (rewriteExpr e) t
    LetDiamond s ty r ps mt e1 e2 ->
      LetDiamond s ty r ps mt (rewriteExpr e1) (rewriteExpr e2)
    TryCatch s ty r e p mt e1 e2 ->
      TryCatch s ty r (rewriteExpr e) p mt (rewriteExpr e1) (rewriteExpr e2)
    _ -> expr

rewriteVal :: Value ev Type -> Value ev Type
rewriteVal val =
  case val of
    Abs funTy p mt e -> Abs funTy p mt (rewriteExpr e)
    Constr a id vs -> Constr a id (map rewriteVal vs)
    Promote a e -> Promote a (rewriteExpr e)
    Pure a e -> Pure a (rewriteExpr e)
    Nec a e -> Nec a (rewriteExpr e)
    Pack s a ty e v k ty' -> Pack s a ty (rewriteExpr e) v k ty'
    TyAbs a v e -> TyAbs a v (rewriteExpr e)
    _ -> val

exprTy :: Expr ev Type -> Type
exprTy (App _ ty _ _ _) = ty
exprTy (Val _ ty _ _) = ty
exprTy (Binop _ ty _ _ _ _) = ty
exprTy (LetDiamond _ ty _ _ _ _ _) = ty
exprTy (Case _ ty _ _ _) = ty
exprTy (Hole _ ty _ _ _) = ty
exprTy (AppTy _ ty _ _ _) = ty
exprTy (TryCatch _ ty _ _ _ _ _ _) = ty
exprTy (Unpack _ ty _ _ _ _ _) = ty
