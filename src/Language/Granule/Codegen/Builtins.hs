{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins where

import LLVM.AST (Operand)
import LLVM.AST.Type (i32)
import LLVM.IRBuilder (MonadIRBuilder, sdiv)
import LLVM.IRBuilder.Instruction (zext)
import LLVM.IRBuilder.Module (MonadModuleBuilder)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type

data Builtin = Builtin {
    builtinId :: String,
    builtinArgTys :: [Type],
    builtinRetTy :: Type,
    builtinImpl :: forall m. (MonadModuleBuilder m, MonadIRBuilder m) => [Operand] -> m Operand}

mkFunType :: [Type] -> Type -> Type
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
