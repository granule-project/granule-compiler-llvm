{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Extras where

import LLVM.AST.Constant
import LLVM.AST.Operand
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

-- div :: Int -> Int -> Int
divDef :: Builtin
divDef =
    Builtin "div" args ret impl
    where
        args = [TyCon (Id "Int" "Int"), TyCon (Id "Int" "Int")]
        ret = TyCon (Id "Int" "Int")
        impl [x, y] = sdiv x y

-- use :: a -> a [1]
useDef :: Specialisable
useDef =
    Specialisable "use" impl
    where
        impl _ [val] = return val

-- TODO: handle @
dropFloatDef :: Builtin
dropFloatDef = Builtin "drop@Float" [tyFloat] tyUnit impl
    where
    impl [val] = return $ ConstantOperand (Struct Nothing False [])
