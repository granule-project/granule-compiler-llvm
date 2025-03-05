module Language.Granule.Codegen.StripAST where

import Data.Bifunctor (Bifunctor (second), bimap)
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Type

-- Strips types which are not currently needed (or handled) by
-- the compiler, to make life easier and debugging simpler. We are
-- stripping Box, Star, Borrow and type quantifiers, but we may
-- wish to reinstate these to help with future optimisation. WIP.

stripAST :: AST ev Type -> AST ev Type
stripAST (AST decls defs imports hidden name) =
  AST decls (map stripDef defs) imports hidden name

stripDef :: Def ev Type -> Def ev Type
stripDef (Def s i b spec el ts) =
  Def s i b spec (stripEquationList el) (stripTypeScheme ts)

stripEquationList :: EquationList ev Type -> EquationList ev Type
stripEquationList (EquationList s v b es) =
  EquationList s v b (map stripEquation es)

stripEquation :: Equation ev Type -> Equation ev Type
stripEquation (Equation s n a b ps e) =
  Equation s n (stripTy a) b (map stripPat ps) (stripExpr e)

stripExpr :: Expr ev Type -> Expr ev Type
stripExpr (App s a b e1 e2) =
  App s (stripTy a) b (stripExpr e1) (stripExpr e2)
stripExpr (Binop s a b op e1 e2) =
  Binop s (stripTy a) b op (stripExpr e1) (stripExpr e2)
stripExpr (LetDiamond s a b p mt e1 e2) =
  LetDiamond s (stripTy a) b (stripPat p) (stripMaybeTy mt) (stripExpr e1) (stripExpr e2)
stripExpr (Val s a b v) =
  Val s (stripTy a) b (stripVal v)
stripExpr (Case s a b e pes) =
  Case s (stripTy a) b (stripExpr e) (map (bimap stripPat stripExpr) pes)
stripExpr (Hole s a b ids hints) =
  Hole s (stripTy a) b ids hints
stripExpr (AppTy s a b e t) =
  AppTy s (stripTy a) b (stripExpr e) (stripTy t)
stripExpr (TryCatch s a b e1 p mt e2 e3) =
  TryCatch s (stripTy a) b (stripExpr e1) (stripPat p) (stripMaybeTy mt) (stripExpr e2) (stripExpr e3)
stripExpr (Unpack s a b tyVar var e1 e2) =
  Unpack s (stripTy a) b tyVar var (stripExpr e1) (stripExpr e2)

stripVal :: Value ev Type -> Value ev Type
stripVal (Var a id) =
  Var (stripTy a) id
stripVal (Abs a p mt e) =
  Abs (stripTy a) (stripPat p) (stripMaybeTy mt) (stripExpr e)
stripVal (Promote a e) =
  Promote (stripTy a) (stripExpr e)
stripVal (Pure a e) =
  Pure (stripTy a) (stripExpr e)
stripVal (Constr a id vs) =
  Constr (stripTy a) id (map stripVal vs)
stripVal (Ext a ev) =
  Ext (stripTy a) ev
stripVal (Nec a e) =
  Nec (stripTy a) (stripExpr e)
stripVal (Pack s a t e id k t') =
  Pack s (stripTy a) (stripTy t) (stripExpr e) id (stripTy k) (stripTy t')
stripVal (TyAbs a (Left (id, t)) e) =
  TyAbs (stripTy a) (Left (id, stripTy t)) (stripExpr e)
stripVal (TyAbs a (Right ids) e) =
  TyAbs (stripTy a) (Right ids) (stripExpr e)
stripVal v = v

stripPat :: Pattern Type -> Pattern Type
stripPat (PVar s a b v) = PVar s (stripTy a) b v
stripPat (PWild s a b) = PWild s (stripTy a) b
stripPat (PBox s a b p) = stripPat p
stripPat (PInt s a b i) = PInt s (stripTy a) b i
stripPat (PFloat s a b f) = PFloat s (stripTy a) b f
stripPat (PConstr s a b id ids ps) = PConstr s (stripTy a) b id ids (map stripPat ps)

stripTypeScheme :: TypeScheme -> TypeScheme
stripTypeScheme (Forall s quants constraints t) =
  Forall
    s
    (map (second stripTy) quants)
    (map stripTy constraints)
    (stripTy t)

stripMaybeTy :: Maybe Type -> Maybe Type
stripMaybeTy Nothing = Nothing
stripMaybeTy (Just ty) = Just (stripTy ty)

stripTy :: Type -> Type
stripTy (Type i) = Type i
stripTy (FunTy id mc arg ret) = FunTy id (stripMaybeTy mc) (stripTy arg) (stripTy ret)
stripTy (TyCon id) = TyCon id
stripTy (Box c t) = stripTy t
stripTy (Diamond e t) = Diamond (stripTy e) (stripTy t)
stripTy (Star g t) = stripTy t
stripTy (Borrow p t) = stripTy t
stripTy (TyVar id) = TyVar id
stripTy (TyApp t1 t2) = TyApp (stripTy t1) (stripTy t2)
stripTy (TyGrade mt i) = TyGrade (stripMaybeTy mt) i
stripTy (TyInfix op t1 t2) = TyInfix op (stripTy t1) (stripTy t2)
stripTy (TySet p ts) = TySet p (map stripTy ts)
stripTy (TyCase t tps) = TyCase (stripTy t) (map (bimap stripTy stripTy) tps)
stripTy (TySig t k) = TySig (stripTy t) (stripTy k)
stripTy (TyExists id k t) = stripTy t
stripTy (TyForall id k t) = stripTy t
stripTy t = t
