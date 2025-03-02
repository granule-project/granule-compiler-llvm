{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Shared where

import LLVM.AST
import LLVM.AST.Type as IR
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

data Builtin = Builtin {
    builtinId :: String,
    builtinArgTys :: [Gr.Type],
    builtinRetTy :: Gr.Type,
    builtinImpl :: forall m. (MonadModuleBuilder m, MonadIRBuilder m) => [Operand] -> m Operand}

-- helpers

mkFunType :: [Gr.Type] -> Gr.Type -> Gr.Type
mkFunType args ret = foldr (FunTy Nothing Nothing) ret args

structTy :: IR.Type
structTy =  StructureType False [i32, ptr IR.double]

tyInt :: Gr.Type
tyInt = TyCon (Id "Int" "Int")

tyFloat :: Gr.Type
tyFloat = TyCon (Id "Float" "Float")

tyChar :: Gr.Type
tyChar = TyCon (Id "Char" "Char")

tyPair :: (Gr.Type, Gr.Type) -> Gr.Type
tyPair (l, r) = TyApp (TyApp (TyCon (Id "," ",")) l) r

tyFloatArray :: Gr.Type
tyFloatArray = TyApp (TyCon (Id "FloatArray" "FloatArray")) (TyVar (Id "id" "id"))
