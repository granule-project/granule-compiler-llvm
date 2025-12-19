module Language.Granule.Codegen.Builtins.Builtins where

import Language.Granule.Codegen.Builtins.Char
import Language.Granule.Codegen.Builtins.Extras
import Language.Granule.Codegen.Builtins.FloatArray
import Language.Granule.Codegen.Builtins.Shared
import Language.Granule.Codegen.Builtins.String
import Language.Granule.Syntax.Identifiers (Id, mkId)

builtins :: [Builtin]
builtins =
  [ charToIntDef,
    charFromIntDef,
    stringAppendDef,
    stringConsDef,
    stringSnocDef,
    divDef,
    newFloatArrayIDef,
    readFloatArrayIDef,
    writeFloatArrayIDef,
    lengthFloatArrayIDef,
    newFloatArrayDef,
    readFloatArrayDef,
    writeFloatArrayDef,
    lengthFloatArrayDef,
    deleteFloatArrayDef
  ]

specialisable :: [Specialisable]
specialisable =
  [ useDef
  ]

monoBuiltinIds :: [Id]
monoBuiltinIds = map (mkId . builtinId) builtins

polyBuiltinIds :: [Id]
polyBuiltinIds = map (mkId . specialisableId) specialisable

builtinIds :: [Id]
builtinIds = monoBuiltinIds ++ polyBuiltinIds
