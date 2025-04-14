module Language.Granule.Codegen.Builtins.Builtins where

import Language.Granule.Codegen.Builtins.Extras
import Language.Granule.Codegen.Builtins.FloatArray
import Language.Granule.Codegen.Builtins.Ref
import Language.Granule.Codegen.Builtins.Session
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Builtins.Uniqueness
import Language.Granule.Syntax.Identifiers (Id, mkId)

builtins :: [Builtin]
builtins =
  [ charToIntDef,
    divDef,
    newFloatArrayIDef,
    readFloatArrayIDef,
    writeFloatArrayIDef,
    lengthFloatArrayIDef,
    newFloatArrayDef,
    readFloatArrayDef,
    writeFloatArrayDef,
    lengthFloatArrayDef,
    deleteFloatArrayDef,
    dropFloatDef
  ]

specialisable :: [Specialisable]
specialisable =
  [ useDef,
    newRefDef,
    freezeRefDef,
    swapRefDef,
    readRefDef,
    uniqueReturnDef,
    revealDef,
    uniqueBindDef,
    trustedBindDef,
    withBorrowDef,
    splitDef,
    joinDef,
    borrowPushDef,
    borrowPullDef,
    forkLinearDef,
    sendDef,
    recvDef,
    closeDef
  ]

monoBuiltinIds :: [Id]
monoBuiltinIds = map (mkId . builtinId) builtins

polyBuiltinIds :: [Id]
polyBuiltinIds = map (mkId . specialisableId) specialisable

builtinIds :: [Id]
builtinIds = monoBuiltinIds ++ polyBuiltinIds
