{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Granule.Codegen.Emit.MainOut where

import LLVM.AST (Operand)
import qualified LLVM.AST as IR
import LLVM.AST.Constant
import qualified LLVM.AST.Constant as C
import LLVM.AST.Name (mkName)
import LLVM.AST.Type
import LLVM.IRBuilder hiding (double)
import Language.Granule.Codegen.ClosureFreeDef
import Language.Granule.Codegen.Emit.LLVMHelpers (intConstant, mkPName)
import Language.Granule.Codegen.Emit.Types (GrType)
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type

data TypeInfo = TypeInfo {
    llvmType :: IR.Type,
    formatStr :: String,
    formatLen :: Int }

getTypeInfo :: GrType -> TypeInfo
getTypeInfo (TyCon (Id "Int" _)) = TypeInfo i32 "%d\n" 4
getTypeInfo (TyCon (Id "Float" _)) = TypeInfo double "%.6f\n" 6
getTypeInfo _ = error "Not supported"

findMainReturnType :: [ClosureFreeValueDef] -> GrType
findMainReturnType defs =
    case filter isMainDef defs of
        [ValueDef _ _ _ (Forall _ _ _ ty)] -> ty
        _ -> error "No main function found"
    where
        isMainDef (ValueDef _ ident _ _) = internalName ident == "main"

loadMainValue :: (MonadModuleBuilder m, MonadIRBuilder m) => GrType -> m Operand
loadMainValue mainTy =
    let info = getTypeInfo mainTy
        mainRef = IR.ConstantOperand $ GlobalReference (ptr $ llvmType info) (mkName "def.main")
    in load mainRef 4

externWrite :: (MonadModuleBuilder m) => GrType -> m Operand
externWrite ty = do
    let info = getTypeInfo ty
    printf <- externVarArgs (mkName "printf") [ptr i8] i32
    function "writeOut" [(llvmType info, mkPName "x")] void $ \[x] -> do
        _ <- globalStringPtr (formatStr info) (mkName ".format")
        let addr = IR.ConstantOperand $ C.GetElementPtr True formatStr [intConstant 0, intConstant 0]
                where formatStr = GlobalReference ty ".format"
                      ty = ptr (ArrayType (fromIntegral $ formatLen info) i8)
        _ <- call printf [(addr, []), (x, [])]
        retVoid

write :: GrType -> Constant
write ty =
  GlobalReference functionType name
    where
        name = mkName "writeOut"
        info = getTypeInfo ty
        functionType = ptr (FunctionType void [llvmType info] False)
