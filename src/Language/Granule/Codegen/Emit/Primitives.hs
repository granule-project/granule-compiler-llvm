module Language.Granule.Codegen.Emit.Primitives where
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import LLVM.AST (mkName, Operand(..))
import LLVM.AST.Constant (Constant, Constant(..))
import LLVM.AST.Type (i1, i8, i32, i64, ptr, void, Type(..))
import LLVM.IRBuilder (MonadModuleBuilder)

malloc :: Constant
malloc = GlobalReference functionType name
         where name = mkName "malloc"
               functionType = ptr (FunctionType (ptr i8) [i64] False)


abort :: Constant
abort = GlobalReference functionType name
        where name = mkName "abort"
              functionType = ptr (FunctionType void [] False)

trap :: (MonadIRBuilder m, MonadModuleBuilder m) => m ()
trap = (call (ConstantOperand abort) []) >> unreachable >> return ()

printf :: Constant
printf = GlobalReference functionType name
        where name = mkName "printf"
              functionType = ptr (FunctionType i32 [ptr i8] True)

memcpy :: Constant
memcpy = GlobalReference functionType name
        where name = mkName "llvm.memcpy.p0.p0.i32"
              functionType = ptr (FunctionType void [ptr i8, ptr i8, i32, i1] False)

free :: Constant
free = GlobalReference functionType name
        where name = mkName "free"
              functionType = ptr (FunctionType void [ptr i8] False)
