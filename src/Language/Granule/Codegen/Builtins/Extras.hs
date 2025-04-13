{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Extras where

import LLVM.AST.Type as IR
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

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

-- use :: a -> a [1]
useDef :: Builtin
useDef =
    Builtin "use" args ret impl
    where
        args = [Gr.tyVar "a"]
        ret = Box (TyGrade Nothing 1) (Gr.tyVar "a")
        impl [val] = return val
