{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.ImmutableArray where

import LLVM.AST
import LLVM.AST.Type as IR
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LLVMHelpers (sizeOf)
import Language.Granule.Codegen.Emit.Primitives (malloc, memcpy)

-- newFloatArrayI :: Int -> FloatArray id
newFloatArrayIDef :: Builtin
newFloatArrayIDef =
    Builtin "newFloatArrayI" args ret impl
    where
        args = [tyInt]
        ret = tyFloatArray
        impl [len] = do
            arrPtr <- call (ConstantOperand malloc) [(ConstantOperand $ sizeOf structTy, [])]
            arrPtr' <- bitcast arrPtr (ptr structTy)
            -- length * double precision 8 bytes
            dataSize <- mul len (int32 8)
            dataPtr <- call (ConstantOperand malloc) [(dataSize, [])]

            lenField <- gep arrPtr' [int32 0, int32 0]
            store lenField 0 len

            dataField <- gep arrPtr' [int32 0, int32 1]
            store dataField 0 dataPtr

            return arrPtr

-- readFloatArrayI :: (FloatArray id) -> Int -> (Float, FloatArray id)
readFloatArrayIDef :: Builtin
readFloatArrayIDef =
    Builtin "readFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt]
        ret = tyPair (tyFloat, tyFloatArray)
        impl [arrPtr, idx] = do
            -- arr -> data -> idx -> val

            arrPtr' <- bitcast arrPtr (ptr structTy)

            dataField <- gep arrPtr' [int32 0, int32 1]
            dataPtr <- load dataField 0

            valuePtr <- gep dataPtr [idx]
            value <- load valuePtr 0

            let pairTy = StructureType False [IR.double, ptr structTy]
            let pair = ConstantOperand $ C.Undef pairTy
            pair' <- insertValue pair value [0]
            insertValue pair' arrPtr [1]



-- writeFloatArrayI :: (FloatArray id) -> Int -> Float -> FloatArray id
writeFloatArrayIDef :: Builtin
writeFloatArrayIDef =
    Builtin "writeFloatArrayI" args ret impl
    where
        args = [tyFloatArray, tyInt, tyFloat]
        ret = tyFloatArray
        impl [arrPtr, idx, val] = do
            arrPtr' <- bitcast arrPtr (ptr structTy)

            lenField <- gep arrPtr' [int32 0, int32 0]
            len <- load lenField 0

            dataField <- gep arrPtr' [int32 0, int32 1]
            dataPtr <- load dataField 0

            -- need to create a new array as in newFloatArrayI
            newArrPtr <- call (ConstantOperand malloc) [(ConstantOperand $ sizeOf structTy, [])]
            newArrPtr' <- bitcast newArrPtr (ptr structTy)

            dataSize <- mul len (int32 8)
            newDataPtr <- call (ConstantOperand malloc) [(dataSize, [])]
            newDataPtr' <- bitcast newDataPtr (ptr IR.double)

            -- copy the existing data to new array
            _ <- call (ConstantOperand memcpy)
                [ (newDataPtr, [])
                , (dataPtr, [])
                , (dataSize, [])
                , (bit 0, [])
                ]

            -- write the val to new copy
            valuePtr <- gep newDataPtr' [idx]
            store valuePtr 0 val

            newLenField <- gep newArrPtr' [int32 0, int32 0]
            store newLenField 0 len

            newDataField <- gep newArrPtr' [int32 0, int32 1]
            store newDataField 0 newDataPtr

            return newArrPtr

-- lengthFloatArrayI :: (FloatArray id) -> (Int -> FloatArray id)
lengthFloatArrayIDef :: Builtin
lengthFloatArrayIDef =
    Builtin "lengthFloatArrayI" args ret impl
    where
        args = [tyFloatArray]
        ret = tyPair (tyInt, tyFloatArray)
        impl [arrPtr] = do
            arrPtr' <- bitcast arrPtr (ptr structTy)

            lenField <- gep arrPtr' [int32 0, int32 0]
            len <- load lenField 0

            let pairTy = StructureType False [i32, ptr structTy]
            let pair = ConstantOperand $ C.Undef pairTy
            pair' <- insertValue pair len [0]
            insertValue pair' arrPtr [1]
