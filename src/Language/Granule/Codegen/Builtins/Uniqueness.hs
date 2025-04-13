{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.Granule.Codegen.Builtins.Uniqueness where

import LLVM.IRBuilder (extractValue)
import LLVM.IRBuilder.Instruction (call)
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Emit.LowerType (llvmType)

uniqueReturnDef, revealDef, uniqueBindDef, trustedBindDef, withBorrowDef, splitDef, joinDef, borrowPushDef, borrowPullDef :: Specialisable
--- *a -> a [r]
uniqueReturnDef =
  Specialisable "uniqueReturn" impl
  where
    impl _ [val] = return val

--- (*a -> b) -> a [r] -> b
uniqueBindDef =
  Specialisable "uniqueBind" impl
  where
    impl _ [fn, val] = do
      functionPtr <- extractValue fn [0]
      environmentPtr <- extractValue fn [1]
      call functionPtr [(environmentPtr, []), (val, [])]

--- a *{Trusted} -> a [Lo]
revealDef =
  Specialisable "reveal" impl
  where
    impl _ [val] = return val

--- (a *{Trusted} -> b [Lo]) -> a [Lo] -> b [Lo]
trustedBindDef =
  Specialisable "trustedBind" impl
  where
    impl _ [fn, val] = do
      functionPtr <- extractValue fn [0]
      environmentPtr <- extractValue fn [1]
      call functionPtr [(environmentPtr, []), (val, [])]

--- (& 1 a -> & 1 b) -> *a -> *b
withBorrowDef =
  Specialisable "withBorrow" impl
  where
    impl _ [fn, val] = do
      functionPtr <- extractValue fn [0]
      environmentPtr <- extractValue fn [1]
      call functionPtr [(environmentPtr, []), (val, [])]

--- & f a -> (& (f * 1/2) a, & (f * 1/2) a)
splitDef =
  Specialisable "split" impl
  where
    impl [valTy] [val] =
      makePair (llvmType valTy, val) (llvmType valTy, val)

--- (& f a, & g a) -> & (f+g) a
joinDef =
  Specialisable "join" impl
  where
    impl _ [pair] =
      extractValue pair [0]

--- & f (a, b) -> (& f a, & f b)
borrowPushDef =
  Specialisable "borrowPush" impl
  where
    impl _ [pair] = do
      return pair

--- (& f a, & f b) -> & f (a, b)
borrowPullDef =
  Specialisable "borrowPull" impl
  where
    impl _ [pair] = do
      return pair
