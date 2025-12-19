{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Session where

import LLVM.AST.Constant
import LLVM.AST.Operand hiding (Undef)
import LLVM.AST.Type as IR
import LLVM.IRBuilder
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LowerType (llvmType)
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Type as Gr

-- TODO
getTy :: Gr.Type -> Gr.Type
getTy (TyApp (TyCon (Id "LChan" "LChan")) (TyApp (TyApp (TyCon (Id "Send" "Send")) ty) (TyCon (Id "End" "End")))) = ty
getTy (TyApp (TyCon (Id "LChan" "LChan")) (TyApp (TyCon (Id "Dual" "Dual")) (TyApp (TyApp (TyCon (Id "Send" "Send")) ty) (TyCon (Id "End" "End"))))) = ty
getTy (FunTy Nothing Nothing (TyApp (TyCon (Id "LChan" "LChan")) (TyApp (TyApp (TyCon (Id "Send" "Send")) ty) (TyCon (Id "End" "End")))) (TyCon (Id "()" "()"))) = ty

-- TODO
chanStruct :: Gr.Type -> IR.Type
chanStruct ty = StructureType False [llvmType ty]

--- (LChan s -> ()) -> LChan (Dual s)
forkLinearDef :: Specialisable
forkLinearDef =
  Specialisable "forkLinear" impl
  where
    impl [fTy] [f] = do
      chan <- allocateStruct (chanStruct $ getTy fTy)
      functionPtr <- extractValue f [0]
      environmentPtr <- extractValue f [1]
      _ <- call functionPtr [(environmentPtr, []), (chan, [])]
      return chan

--- (LChan (Send a s)) -> a -> LChan s
sendDef :: Specialisable
sendDef =
  Specialisable "send" impl
  where
    impl [ct, ft] [c, f] = do
      writeStruct c 0 f
      return c

--- (LChan (Recv a s)) -> (a, LChan s)
recvDef :: Specialisable
recvDef =
  Specialisable "recv" impl
  where
    impl [ct] [c] = do
      let ty = getTy ct
      val <- readStruct c 0
      makePair (llvmType ty, val) (ptr $ chanStruct ty, c)

--- (LChan End) -> ()
closeDef :: Specialisable
closeDef =
  Specialisable "close" impl
  where
    impl [cTy] [c] = do
      -- cleanup c
      return $ ConstantOperand (Struct Nothing False [])
