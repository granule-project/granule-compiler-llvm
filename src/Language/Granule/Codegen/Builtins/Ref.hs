{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Ref where

import LLVM.AST.Type as IR
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LowerType (llvmType, refInnerTy)
import Language.Granule.Syntax.Type as Gr

newRefDef, swapRefDef, freezeRefDef, readRefDef :: Specialisable
newRefDef =
  Specialisable "newRef" impl
  where
    impl [valTy] [val] = do
      refPtr <- allocateStruct (refStruct valTy)
      writeStruct refPtr 0 val
      return refPtr
swapRefDef =
  Specialisable "swapRef" impl
  where
    impl [refTy, valTy] [refPtr, newVal] = do
      prev <- readStruct refPtr 0
      writeStruct refPtr 0 newVal
      makePair (llvmType valTy, prev) (ptr (refStruct valTy), refPtr)
freezeRefDef =
  Specialisable "freezeRef" impl
  where
    impl _ [refPtr] = do
      val <- readStruct refPtr 0
      _ <- free refPtr
      return val
readRefDef =
  Specialisable "readRef" impl
  where
    impl [refTy] [refPtr] = do
      val <- readStruct refPtr 0
      makePair (refInnerTy refTy, val) (ptr (llvmType refTy), refPtr)

refStruct :: Gr.Type -> IR.Type
refStruct ty = StructureType False [llvmType ty]
