module Language.Granule.Codegen.RewriteAST where

import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type
import Language.Granule.Codegen.RetypeAST

-- Rewrite Unpack ASTs into App Abs ASTs which our
-- compiler already knows how to handle. WIP.

rewriteAST :: AST ev Type -> AST ev Type
rewriteAST ast = ast {definitions = map rewriteDef (definitions ast)}
  where
    rewriteDef def = def {defEquations = rewriteEquationList (defEquations def)}
    rewriteEquationList eqs = eqs {equations = map rewriteEquation (equations eqs)}
    rewriteEquation eq = eq {equationBody = rewriteExpr (equationBody eq)}

rewriteExpr :: Expr ev Type -> Expr ev Type
rewriteExpr (Unpack s retTy b tyVar var e1 e2) =
  let e1' = rewriteExpr e1
      e1Ty = exprTy e1'
      e2' = rewriteExpr e2
      absTy = FunTy Nothing Nothing e1Ty retTy
   in fixTypes (App s retTy b (Val s absTy b (Abs absTy (PVar s e1Ty b var) Nothing e2')) e1')
   where
    fixTypes expr = snd $ retypeExpr emptyEnv expr
rewriteExpr (App s a b e1 e2) = App s a b (rewriteExpr e1) (rewriteExpr e2)
rewriteExpr (Val s a b v) = Val s a b (rewriteVal v)
rewriteExpr exp = exp

rewriteVal :: Value ev Type -> Value ev Type
rewriteVal (Abs a p mt e) = Abs a p mt (rewriteExpr e)
rewriteVal val = val

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
