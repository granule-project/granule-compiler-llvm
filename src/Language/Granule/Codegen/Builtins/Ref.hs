{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Ref where

import LLVM.AST.Type as IR
import Language.Granule.Codegen.Builtins.Shared

newRefDef, swapRefDef, freezeRefDef, readRefDef :: Builtin
newRefDef =
  Builtin "__newRef_1337257197337028798" [tyFloat] tyFloatRef impl
  where
    impl [val] = do
      refPtr <- allocateStruct floatRefStruct
      writeStruct refPtr 0 val
      return refPtr
swapRefDef =
  Builtin "__swapRef_4785688958494461927" [tyFloatRef, tyFloat] (tyPair (tyFloat, tyFloatRef)) impl
  where
    impl [refPtr, newVal] = do
      prev <- readStruct refPtr 0
      writeStruct refPtr 0 newVal
      makePair (IR.double, prev) (ptr floatRefStruct, refPtr)
freezeRefDef =
  Builtin "__freezeRef_4785688958494461927" [tyFloatRef] tyFloat impl
  where
    impl [refPtr] = do
      val <- readStruct refPtr 0
      _ <- free refPtr
      return val
readRefDef =
  Builtin "__readRef_4785688958494461927" [tyFloatRef] (tyPair (tyFloat, tyFloatRef)) impl
  where
    impl [refPtr] = do
      val <- readStruct refPtr 0
      makePair (IR.double, val) (ptr floatRefStruct, refPtr)

floatRefStruct :: IR.Type
floatRefStruct = refStruct IR.double

refStruct :: IR.Type -> IR.Type
refStruct ty = StructureType False [ty]
