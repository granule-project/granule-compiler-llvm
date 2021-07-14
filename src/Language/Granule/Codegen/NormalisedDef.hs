{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

{-| This transform makes every definition have a 0-1
    arguments via currying, using lambdas where appropriate.

    For example
    @
        f x y = something x y
        f : Int -> Int -> Int -> Int
    @
    becomes
    @
        f : Int -> Int -> Int -> Int
        f x = (\y -> something x y)
    @

    likewise
    @
        f : Int -> Int -> Int
        f = (\x -> (\y -> x + y))
    @
    becomes
    @
        f : Int -> Int -> Int
        f x = (\y -> x + y)
    @

    It also elimitates refutable argument pattern matches, converting
    them to case expressions.

    e.g.
    @
       xor 1 1 = True
       xor 0 0 = True
       xor x y = False
    @

    @
       xor x y =
          case (x, y) of
            (1, 1) -> True
            (0, 0) -> True
            (x, y) -> False
  |-}
module Language.Granule.Codegen.NormalisedDef where

import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Pattern
import Language.Granule.Syntax.Annotated
import Language.Granule.Syntax.Pretty
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Type
import Language.Granule.Syntax.Span
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.FirstParameter
import Data.Either (lefts, rights)
import Data.List (transpose, intercalate)
import GHC.Generics

data NormalisedAST v a =
    NormalisedAST [DataDecl] [FunctionDef v a] [ValueDef v a]

instance (Pretty a) => Pretty (NormalisedAST a v) where
    pretty (NormalisedAST dataDecls functionDefs valueDefs) =
        pretty' dataDecls <> "\n\n" <> pretty' functionDefs <> pretty' valueDefs
        where
            pretty' :: Pretty l => [l] -> String
            pretty' = intercalate "\n\n" . map pretty

deriving instance (Show a, Show v) => Show (NormalisedAST v a)
deriving instance (Eq a, Eq v) => Eq (NormalisedAST v a)

data ValueDef v a =
    ValueDef {
        valueDefSpan :: Span,
        valueDefIdentifier :: Id,
        valueDefInitializer :: Expr v a,
        valueDefTypeScheme :: TypeScheme }
    deriving Generic
deriving instance (Show a, Show v) => Show (ValueDef v a)
deriving instance (Eq a, Eq v) => Eq (ValueDef v a)

data FunctionDef v a =
    FunctionDef {
        functionDefSpan :: Span,
        functionDefIdentifier :: Id,
        functionDefBody :: Expr v a,
        functionDefArgument :: Pattern a,
        functionDefTypeScheme :: TypeScheme }
    deriving Generic
deriving instance (Show a, Show v) => Show (FunctionDef v a)
deriving instance (Eq a, Eq v) => Eq (FunctionDef v a)

instance (Pretty v) => Pretty (ValueDef v a) where
    pretty (ValueDef _ v e t) = pretty v <> " : " <> pretty t <> "\n" <>
                                   pretty v <> " = " <> pretty e

instance Pretty v => Pretty (FunctionDef v a) where
    pretty (FunctionDef _ v e ps t) = pretty v <> " : " <> pretty t <> "\n" <>
                                         pretty v <> " " <> pretty ps <> "= " <> pretty e

instance FirstParameter (ValueDef v a) Span

normaliseDefinitions :: AST ev Type -> NormalisedAST ev Type
normaliseDefinitions (AST dd defs imports _ _) =
    let normalisedDefs = map normaliseDefinition defs
    in NormalisedAST dd (lefts normalisedDefs) (rights normalisedDefs)

normaliseDefinition :: Def ev Type -> Either (FunctionDef ev Type) (ValueDef ev Type)
normaliseDefinition def  =
    let singleEquationDef = makeSingleEquationWithCase def
        equationList      = defEquations singleEquationDef
        equation          = normaliseEquation (head $ equations equationList)
        normalisedDef     = singleEquationDef { defEquations = equationList { equations = [equation] } }
    in case normalisedDef of
           d | isValueDef d    -> Right $ toValueDef normalisedDef
           d | isFunctionDef d -> Left  $ toFunctionDef normalisedDef
           _ -> error "Unrecognised Def"

isFunctionDef :: Def v a -> Bool
isFunctionDef = not . isValueDef

isValueDef :: Def v a -> Bool
isValueDef Def { defEquations = EquationList { equations = [equation] } } =
    null $ equationPatterns equation
isValueDef _ = False

toValueDef :: Def v a -> ValueDef v a
toValueDef (Def sp ident _rf EquationList { equations = [equation] } ts) =
    ValueDef {
        valueDefSpan = sp,
        valueDefIdentifier = ident,
        valueDefInitializer = equationBody equation,
        valueDefTypeScheme = ts }
toValueDef _ = error "toValueDef requires Def with one equation"

toFunctionDef :: Def ev a -> FunctionDef ev a
toFunctionDef (Def sp ident _rf EquationList { equations = [caseEquation] } ts) =
    FunctionDef {
        functionDefSpan = sp,
        functionDefIdentifier = ident,
        functionDefBody = equationBody caseEquation,
        functionDefArgument = head $ equationPatterns caseEquation,
        functionDefTypeScheme = ts }
toFunctionDef _ = error "toFunctionDef requires Def with one equation"

isTriviallyIrrefutable :: Pattern Type -> Bool
isTriviallyIrrefutable
    = patternFold
          (\_ _ _ _  -> True)         -- PVar
          (\_ _ _    -> True)         -- PWild
          (\_ _ _ ch -> True)         -- PBox
          (\_ _ _ _  -> False)        -- PInt
          (\_ _ _ _  -> False)        -- PFloat
          (\_ _ _ _ args -> and args) -- PConstr

hasTriviallyIrrefutableMatch :: Equation ev Type -> Bool
hasTriviallyIrrefutableMatch Equation { equationPatterns = patterns }
    = all isTriviallyIrrefutable patterns

makeSingleEquationWithCase :: Def ev Type -> Def ev Type
makeSingleEquationWithCase def@(Def sp ident _rf EquationList { equations = [eq] } ts)
    | hasTriviallyIrrefutableMatch eq = def
makeSingleEquationWithCase def@(Def sp ident _rf eqs@EquationList { equations = eqls } ts) =
    let equation = Equation sp eqIdent (definitionType def) False irrefutableArgs generatedCaseExpr
                   where irrefutableArgs     = makeIrrefutableArgs casePatterns
                         generatedCaseExpr   = makeCaseExpr irrefutableArgs (casePatterns, caseExprs)
                         casePatterns        = map equationPatterns eqls
                         caseExprs           = map equationBody eqls
                         eqIdent             = equationsId eqs
    in def { defEquations = eqs { equations = [equation] } }

makeIrrefutableArgs :: [[Pattern Type]] -> [Pattern Type]
makeIrrefutableArgs patternLists =
    zipWith patternForArg [1..] (transpose patternLists)

-- | Assumes best name is the last in the list.
-- | If there are no simple var matches the name is unnamed.n
patternForArg :: Int -> [Pattern Type] -> Pattern Type
patternForArg n patterns =
    let patternTy = annotation $ head patterns
        defaultPattern = PVar nullSpanNoFile patternTy False (mkId $ "unnamed." ++ show n)
        accumulateBestName irrefutablePat@(PVar _ _ _ ident) bestName = irrefutablePat
        accumulateBestName reffutablePat bestName                   = bestName
    in foldr accumulateBestName defaultPattern patterns

makeCaseExpr :: [Pattern Type]
             -> ([[Pattern Type]], [Expr ev Type])
             -> Expr ev Type
makeCaseExpr irrefutableArgPatterns (casePatterns, caseExprs) =
    Case nullSpanNoFile ty False (Val nullSpanNoFile (annotation sw) False sw) branches
    where branches       = zip (map mergePatterns casePatterns) caseExprs
          ty             = annotation (head caseExprs)
          sw         = mergeArguments $ boundVarsAndAnnotations $
                               mergePatterns irrefutableArgPatterns

-- | x, y, z -> ((x, y), z)
mergePatterns :: [Pattern Type] -> Pattern Type
mergePatterns (firstPattern:remainingPatterns) =
    foldl patternPair firstPattern remainingPatterns
    where patternPair left right = ppair nullSpanNoFile (pairType (annotation left) (annotation right)) left right
          patternPair :: Pattern Type -> Pattern Type -> Pattern Type
mergePatterns [] = error "One or more patterns required"

mergeArguments :: [(Type, Id)] -> Value ev Type
mergeArguments argumentsIds =
  case map (uncurry Var) argumentsIds of
    firstArg:otherArgs -> foldl typedPair firstArg otherArgs
    _ -> error "Cannot merge less than two arguments"

normaliseEquation :: Equation ev Type -> Equation ev Type
normaliseEquation eq@Equation { equationPatterns = [] } = tryHoistLambda eq
normaliseEquation eq@Equation { equationPatterns = [_] } = eq
normaliseEquation eq = curryEquation eq

tryHoistLambda :: Equation ev Type -> Equation ev Type
tryHoistLambda eq@Equation { equationBody = (Val _ _ _ (Abs _ arg _ ex)) } =
    eq { equationPatterns = [arg], equationBody = ex }
tryHoistLambda def = def

curryEquation :: Equation ev Type -> Equation ev Type
curryEquation eq =
    case equationPatterns eq of
      eqArg:otherArgs ->
        let body = equationBody eq
            body' = argsToLambda otherArgs body (equationAnnotation eq)
        in eq { equationPatterns = [eqArg], equationBody = body' }
      [] -> error "Cannot curry no-arg equation"

argsToLambda :: [Pattern Type] -> Expr ev Type -> Type -> Expr ev Type
argsToLambda args originalBody ty =
    foldr wrapInLambda originalBody args
    where sp = getSpan originalBody
          wrapInLambda arg body = let bodyType = annotation body
                                      absType = funTy (annotation arg) bodyType
                                  in Val sp absType False (Abs absType arg Nothing body)
