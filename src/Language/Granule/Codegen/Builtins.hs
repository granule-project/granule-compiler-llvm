{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins where

import LLVM.AST (Operand)
import LLVM.AST.Type as IR
import LLVM.IRBuilder (MonadIRBuilder, sdiv)
import LLVM.IRBuilder.Instruction (zext)
import LLVM.IRBuilder.Module (MonadModuleBuilder)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

data Builtin = Builtin {
    builtinId :: String,
    builtinArgTys :: [Gr.Type],
    builtinRetTy :: Gr.Type,
    builtinImpl :: forall m. (MonadModuleBuilder m, MonadIRBuilder m) => [Operand] -> m Operand}

mkFunType :: [Gr.Type] -> Gr.Type -> Gr.Type
mkFunType args ret = foldr (FunTy Nothing Nothing) ret args

builtins :: [Builtin]
builtins = [charToIntDef, divDef]

builtinIds :: [Id]
builtinIds = map (mkId . builtinId) builtins

-- charToInt :: Char -> Int
charToIntDef :: Builtin
charToIntDef =
    Builtin "charToInt" args ret impl
    where
        args = [TyCon (Id "Char" "Char")]
        ret = TyCon (Id "Int" "Int")
        impl [x] = zext x i32

-- div :: Int -> Int -> Int
divDef :: Builtin
divDef =
    Builtin "div" args ret impl
    where
        args = [TyCon (Id "Int" "Int"), TyCon (Id "Int" "Int")]
        ret = TyCon (Id "Int" "Int")
        impl [x, y] = sdiv x y

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
