{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.MutableArray where

import LLVM.AST
import LLVM.AST.Type as IR
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Constant as C
import LLVM.IRBuilder.Instruction
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LLVMHelpers (sizeOf)
import Language.Granule.Codegen.Emit.Primitives (malloc, free)

-- newFloatArray :: Int -> FloatArray id
newFloatArrayDef :: Builtin
newFloatArrayDef =
    Builtin "newFloatArray" args ret impl
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

-- readFloatArray :: (FloatArray id) -> Int -> (Float, FloatArray id)
readFloatArrayDef :: Builtin
readFloatArrayDef =
    Builtin "readFloatArray" args ret impl
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



-- writeFloatArray :: (FloatArray id) -> Int -> Float -> FloatArray id
writeFloatArrayDef :: Builtin
writeFloatArrayDef =
    Builtin "writeFloatArray" args ret impl
    where
        args = [tyFloatArray, tyInt, tyFloat]
        ret = tyFloatArray
        impl [arrPtr, idx, val] = do
            arrPtr' <- bitcast arrPtr (ptr structTy)

            dataField <- gep arrPtr' [int32 0, int32 1]
            dataPtr <- load dataField 0

            valuePtr <- gep arrPtr' [idx]
            store valuePtr 0 val

            return arrPtr

-- lengthFloatArray :: (FloatArray id) -> (Int -> FloatArray id)
lengthFloatArrayDef :: Builtin
lengthFloatArrayDef =
    Builtin "lengthFloatArray" args ret impl
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

deleteFloatArrayDef :: Builtin
deleteFloatArrayDef =
    Builtin "deleteFloatArray" args ret impl
        where
            args = [tyFloatArray]
            ret = tyUnit
            impl [arrPtr] = do
                arrPtr' <- bitcast arrPtr (ptr structTy)
                dataField <- gep arrPtr' [int32 0, int32 1]
                dataPtr <- load dataField 0

                -- free the data
                _ <- call (ConstantOperand free) [(dataPtr, [])]

                -- free the array pair
                _ <- call (ConstantOperand free) [(arrPtr, [])]

                -- return unit (need to check)
                return $ ConstantOperand (C.Struct Nothing False [])
