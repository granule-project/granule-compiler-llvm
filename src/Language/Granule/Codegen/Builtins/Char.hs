{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Char where

import LLVM.AST.Type (i32, i8)
import LLVM.IRBuilder.Instruction (trunc, zext)
import Language.Granule.Codegen.Builtins.Shared

charToIntDef, charFromIntDef :: Builtin
charToIntDef =
  Builtin "charToInt" [tyChar] tyInt impl
  where
    impl [x] = zext x i32
charFromIntDef =
  Builtin "charFromInt" [tyInt] tyChar impl
  where
    impl [x] = trunc x i8
