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
