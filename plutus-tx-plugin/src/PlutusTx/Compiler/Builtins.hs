{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions for compiling Plutus Core builtins.
module PlutusTx.Compiler.Builtins (
    builtinNames
    , defineBuiltinTypes
    , defineBuiltinTerms
    , lookupBuiltinTerm
    , lookupBuiltinType
    , errorTy
    , errorFunc) where

<<<<<<< HEAD
import qualified PlutusTx.Builtins.Class       as Builtins
import qualified PlutusTx.Builtins.Internal    as Builtins

import           PlutusTx.Compiler.Error
import           PlutusTx.Compiler.Names
import           PlutusTx.Compiler.Types
import           PlutusTx.Compiler.Utils
import           PlutusTx.PIRTypes

import qualified PlutusIR                      as PIR
import qualified PlutusIR.Compiler.Definitions as PIR
import           PlutusIR.Compiler.Names
import qualified PlutusIR.MkPir                as PIR
import qualified PlutusIR.Purity               as PIR

import qualified PlutusCore                    as PLC
import qualified PlutusCore.Constant           as PLC
import           PlutusCore.Quote

import qualified GhcPlugins                    as GHC

import qualified Language.Haskell.TH.Syntax    as TH

import           Control.Monad.Reader

import qualified Data.ByteString               as BS
import qualified Data.Map                      as Map
import           Data.Proxy
||||||| parent of ac6d6c898 (WIP)
import                qualified PlutusTx.Builtins             as Builtins
import                qualified PlutusTx.String               as String

import                          PlutusTx.Compiler.Error
import {-# SOURCE #-}           PlutusTx.Compiler.Expr
import                          PlutusTx.Compiler.Laziness
import                          PlutusTx.Compiler.Names
import {-# SOURCE #-}           PlutusTx.Compiler.Type
import                          PlutusTx.Compiler.Types
import                          PlutusTx.Compiler.Utils
import                          PlutusTx.PIRTypes

import                qualified PlutusIR                      as PIR
import                qualified PlutusIR.Compiler.Definitions as PIR
import                          PlutusIR.Compiler.Names
import                qualified PlutusIR.MkPir                as PIR
import                qualified PlutusIR.Purity               as PIR

import                qualified PlutusCore                    as PLC
import                qualified PlutusCore.Constant           as PLC
import                          PlutusCore.Quote
import                qualified PlutusCore.StdLib.Data.Bool   as Bool
import                qualified PlutusCore.StdLib.Data.Unit   as Unit

import                qualified GhcPlugins                    as GHC

import                qualified Language.Haskell.TH.Syntax    as TH

import                          Control.Monad
import                          Control.Monad.Reader

import                qualified Data.ByteString               as BS
import                qualified Data.Map                      as Map
import                          Data.Proxy
import                qualified Data.Set                      as Set
=======
import                qualified PlutusTx.Builtins             as Builtins
import                qualified PlutusTx.String               as String

import                          PlutusTx.Compiler.Error
import {-# SOURCE #-}           PlutusTx.Compiler.Expr
import                          PlutusTx.Compiler.Laziness
import                          PlutusTx.Compiler.Names
import {-# SOURCE #-}           PlutusTx.Compiler.Type
import                          PlutusTx.Compiler.Types
import                          PlutusTx.Compiler.Utils
import                          PlutusTx.PIRTypes

import                qualified PlutusIR                      as PIR
import                qualified PlutusIR.Compiler.Definitions as PIR
import                          PlutusIR.Compiler.Names
import                qualified PlutusIR.MkPir                as PIR
import                qualified PlutusIR.Purity               as PIR

import                qualified PlutusCore                    as PLC
import                qualified PlutusCore.Constant           as PLC
import                qualified PlutusCore.Data               as PLC
import                          PlutusCore.Quote
import                qualified PlutusCore.StdLib.Data.Bool   as Bool
import                qualified PlutusCore.StdLib.Data.List   as List
import                qualified PlutusCore.StdLib.Data.Pair   as Pair
import                qualified PlutusCore.StdLib.Data.Unit   as Unit

import                qualified GhcPlugins                    as GHC

import                qualified Language.Haskell.TH.Syntax    as TH

import                          Control.Monad
import                          Control.Monad.Reader

import                qualified Data.ByteString               as BS
import                qualified Data.Map                      as Map
import                          Data.Proxy
import                qualified Data.Set                      as Set
>>>>>>> ac6d6c898 (WIP)

{- Note [Mapping builtins]
We want the user to be able to call the Plutus builtins as normal Haskell functions.

To do this, we provide a library of such functions in Haskell, and we define corresponding
functions and types in PIR so that we can translate references to the Haskell functions and
types into references to the PIR ones.

We can then do whatever we need to inside the definitions to line things up with the real builtins.
(See Note [Builtin types and Haskell types])

To do this, we first need a map from the Haskell TH names to the corresponding GHC names
(in fact the TyThings, so we have the types too). Annoyingly, this has to be done in the
GHC Core monad and then passed to us.

This map lets us write code that defines all the builtins (by their TH names), and also to look
up things by their TH names in the few other cases where we need to (mostly where we use specific
known builtins to implement primitives).

This is a bit fragile, since we rely on having all the names that we need, and having them
mapped to the right things (GHC will panic on us if we e.g. get the wrong kind of TyThing for
a name). We should probably revisit this later.
-}

{- Note [Builtin types and Haskell types]
Several of the PLC builtins use types that should (morally) line up with types that we compile from
Haskell (see also Note [Which types map to builtin types?]).
But there is a problem: they use either primitive or Scott-encoded versions of these types,
whereas when we compile them from Haskell they will end up as abstract types, and so the types
won't line up at the call site.

That is, we generate something like this:
(/\ (Integer :: *) .
  (\ addInteger : Integer -> Integer -> Integer .
      <use addInteger>
  )
  (\ x,y : Integer . <builtin addInteger> x y) -- Uh oh, type error, can't show that Integer = <builtin int>!
)
{<builtin int>}

We handle this in two different ways:
- For the types like Bool and Unit which are really algebraic types, and which have constructors etc.
that we care about elsewhere, we insert adaptor code into the definition of the builtin (see note [Mapping builtins]).
- For types like Integer and Bytestring which don't have visible constructors, we can treat them as completely opaque,
and we use a transparent type alias. (Actually we fake the alias by actually just substituting the definition in
everywhere until we have aliases in PIR. Bear this in mind for the examples below.)

Here's how that looks for a primitive that takes Ints and returns a Boolean, assuming we have bound Integer and Bool
already as an alias and an abstract type respectively:
(\ equalsInteger : Integer -> Integer -> Bool .
  <use equalsInteger>
)
(\ x, y : Integer . -- No need to do anything to the arguments, since we're using a transparent alias for Int
  (<builtin equalsInteger> x y) {Bool} True False -- Immediately match the builtin bool into an abstract Bool
)

We *could* do something like the interleaved definitions that we do for datatypes in PIR. Morally this is perhaps the
right thing to do: we should think of Integer and its builtins as a "module" that goes together and where all the definitions
have access to the internals of the other definitions. A datatype definition is then a special case of a module definition.
However, for the moment this would be quite a bit more design work and so we leave it for future work.

For an example of how the "abstract module" approach would look:
(/\ (Integer :: *) .
  (\ addInteger : Integer -> Integer -> Integer . -- Type signature is fine inside the abstraction
      <use addInteger>
  )
)
{<builtin int>}
(\ x,y : <builtin int> . <builtin addInteger> x y) -- No type error any more, abstraction is gone
-}

{- Note [Which types map to builtin types?]
We have (will have) Bool in the default builtin universe. Why do we not map the Haskell Bool type to the
builtin Bool, but rather compile it as a normal ADT?

The advantage of mapping a type to a builtin type is mainly performance:
- We can directly use (potentially optimized) implementations of operations on that type.
- We do not need adaptors to interoperate with builtin functions that use the builtin version of the type.

On the other hand, the advantages of *not* doing this are:
- User-written code that operates on the type as normal (e.g. pattern matching) will work.
    - We could make this work by compiling pattern matching specially for the builtin type, but this means
      more special cases in the compiler (boo). Maybe we can do this generically in future.
- Code that uses these types will also be compilable/runnable if those builtins are not present.

Overall, this means that we only map performance-critical types like Integer and ByteString directly to
builtin types, and the others we compile normally.
-}

{- Note [Builtin terms and values]
When generating let-bindings, we would like to generate strict bindings only for things that are obviously
pure, and non-strict bindings otherwise. This ensures that we won't evaluate the RHS of the binding prematurely,
which matters if it could trigger an error, or some other effect.

Additionally, strict bindings are a bit more efficient than non-strict ones (non-strict ones get turned into
lambdas from unit and forcing in the body). So we would like to use strict bindings where possible.

Now, we generate bindings for all our builtin functions... but they are not *obviously* pure!
Fortunately, we have a more sophisticated purity check that also detects unsaturated builtin applications,
which handles these cases too.
-}

mkBuiltin :: fun -> PIR.Term tyname name uni fun ()
mkBuiltin = PIR.Builtin ()

-- | The 'TH.Name's for which 'BuiltinNameInfo' needs to be provided.
builtinNames :: [TH.Name]
builtinNames = [
<<<<<<< HEAD
      ''BS.ByteString
||||||| parent of ac6d6c898 (WIP)
      ''Builtins.ByteString
    , ''Integer
    , ''Bool
    , ''()

=======
      ''Builtins.ByteString
    , ''Builtins.Data
    , ''Integer
    , ''Bool
    , ''()
    , ''(,)
    , ''[]

>>>>>>> ac6d6c898 (WIP)
    , 'Builtins.concatenate
    , 'Builtins.takeByteString
    , 'Builtins.dropByteString
    , 'Builtins.sha2_256
    , 'Builtins.sha3_256
    , 'Builtins.equalsByteString
    , 'Builtins.lessThanByteString
    , 'Builtins.greaterThanByteString
    , 'Builtins.emptyByteString
    , 'Builtins.decodeUtf8

    , 'Builtins.verifySignature

    , ''Integer
    , 'Builtins.addInteger
    , 'Builtins.subtractInteger
    , 'Builtins.multiplyInteger
    , 'Builtins.divideInteger
    , 'Builtins.modInteger
    , 'Builtins.quotientInteger
    , 'Builtins.remainderInteger
    , 'Builtins.greaterThanInteger
    , 'Builtins.greaterThanEqInteger
    , 'Builtins.lessThanInteger
    , 'Builtins.lessThanEqInteger
    , 'Builtins.equalsInteger

    , 'Builtins.error

<<<<<<< HEAD
    , ''Builtins.BuiltinString
||||||| parent of ac6d6c898 (WIP)
    , ''Builtins.String
=======
    , 'Builtins.chooseData
    , 'Builtins.equalsData
    , 'Builtins.unsafeDataAsConstr
    , 'Builtins.unsafeDataAsMap
    , 'Builtins.unsafeDataAsList
    , 'Builtins.unsafeDataAsI
    , 'Builtins.unsafeDataAsB

    , ''Builtins.String
>>>>>>> ac6d6c898 (WIP)
    , ''Char
    , 'Builtins.appendString
    , 'Builtins.emptyString
    , 'Builtins.charToString
    , 'Builtins.equalsString
    , 'Builtins.encodeUtf8
    -- This one is special
    , 'Builtins.stringToBuiltinString

    , 'Builtins.trace
<<<<<<< HEAD

    , ''Builtins.BuiltinBool
    , 'Builtins.ifThenElse
    , 'Builtins.true
    , 'Builtins.false

    , ''Builtins.BuiltinUnit
    , 'Builtins.unitval
    , 'Builtins.chooseUnit

    , ''Builtins.BuiltinPair
    , 'Builtins.fst
    , 'Builtins.snd

    , ''Builtins.BuiltinList
    , 'Builtins.null
    , 'Builtins.head
    , 'Builtins.tail
||||||| parent of ac6d6c898 (WIP)
=======

    , 'builtinBoolToBool
    , 'builtinListToListWithMap
    , 'builtinPairToPair
>>>>>>> ac6d6c898 (WIP)
    ]

-- | Get the 'GHC.TyThing' for a given 'TH.Name' which was stored in the builtin name info,
-- failing if it is missing.
getThing :: Compiling uni fun m => TH.Name -> m GHC.TyThing
getThing name = do
    CompileContext{ccBuiltinNameInfo=names} <- ask
    case Map.lookup name names of
        Nothing    -> throwSd CompilationError $ "Missing builtin name:" GHC.<+> (GHC.text $ show name)
        Just thing -> pure thing

<<<<<<< HEAD
defineBuiltinTerm :: Compiling uni fun m => TH.Name -> PIRTerm uni fun -> m ()
defineBuiltinTerm name term = do
||||||| parent of ac6d6c898 (WIP)
defineBuiltinTerm :: Compiling uni fun m => TH.Name -> PIRTerm uni fun -> [GHC.Name] -> m ()
defineBuiltinTerm name term deps = do
=======
defineTermWithType :: Compiling uni fun m => TH.Name -> PIRType uni -> PIRTerm uni fun -> [GHC.Name] -> m ()
defineTermWithType name ty term deps = do
    ghcId <- GHC.tyThingId <$> getThing name
    n <- compileNameFresh $ GHC.getName ghcId
    -- See Note [Builtin terms and values]
    let strictness = if PIR.isPure (const PIR.NonStrict) term then PIR.Strict else PIR.NonStrict
        vd = PIR.VarDecl () n ty
        def = PIR.Def vd (term, strictness)
    PIR.defineTerm (LexName $ GHC.getName ghcId) def (Set.fromList $ LexName <$> deps)

defineBuiltinTerm :: Compiling uni fun m => TH.Name -> PIRTerm uni fun -> [GHC.Name] -> m ()
defineBuiltinTerm name term deps = do
>>>>>>> ac6d6c898 (WIP)
    ghcId <- GHC.tyThingId <$> getThing name
    var <- compileVarFresh ghcId
    -- See Note [Builtin terms and values]
    let strictness = if PIR.isPure (const PIR.NonStrict) term then PIR.Strict else PIR.NonStrict
        def = PIR.Def var (term, strictness)
    PIR.defineTerm (LexName $ GHC.getName ghcId) def mempty

-- | Add definitions for all the builtin types to the environment.
defineBuiltinType :: forall uni fun m. Compiling uni fun m => TH.Name -> PIRType uni -> m ()
defineBuiltinType name ty = do
    tc <- GHC.tyThingTyCon <$> getThing name
    var <- compileTcTyVarFresh tc
    PIR.defineType (LexName $ GHC.getName tc) (PIR.Def var ty) mempty
    -- these are all aliases for now
    PIR.recordAlias @LexName @uni @fun @() (LexName $ GHC.getName tc)

-- | Add definitions for all the builtin terms to the environment.
defineBuiltinTerms :: CompilingDefault uni fun m => m ()
defineBuiltinTerms = do
<<<<<<< HEAD

    -- See Note [Builtin terms and values]
    -- Bool
    defineBuiltinTerm 'Builtins.ifThenElse $ mkBuiltin PLC.IfThenElse
    defineBuiltinTerm 'Builtins.true $ PIR.mkConstant () True
    defineBuiltinTerm 'Builtins.false $ PIR.mkConstant () False
||||||| parent of ac6d6c898 (WIP)
    bs <- GHC.getName <$> getThing ''Builtins.ByteString
    int <- GHC.getName <$> getThing ''Integer
    bool <- GHC.getName <$> getThing ''Bool
    unit <- GHC.getName <$> getThing ''()
    str <- GHC.getName <$> getThing ''Builtins.String
    char <- GHC.getName <$> getThing ''Char

    intTy <- lookupBuiltinType ''Integer
    bsTy <- lookupBuiltinType ''Builtins.ByteString
    strTy <- lookupBuiltinType ''Builtins.String
=======
    bs <- GHC.getName <$> getThing ''Builtins.ByteString
    dat <- GHC.getName <$> getThing ''Builtins.Data
    int <- GHC.getName <$> getThing ''Integer
    bool <- GHC.getName <$> getThing ''Bool
    unit <- GHC.getName <$> getThing ''()
    pair <- GHC.getName <$> getThing ''(,)
    str <- GHC.getName <$> getThing ''Builtins.String
    char <- GHC.getName <$> getThing ''Char
    lst <- GHC.getName <$> getThing ''[]
    builtinb2b <- GHC.getName <$> getThing 'builtinBoolToBool
    builtinl2l <- GHC.getName <$> getThing 'builtinListToListWithMap
    builtinp2p <- GHC.getName <$> getThing 'builtinPairToPair

    boolTy <- compileType GHC.boolTy
    listTy <- compileTyCon GHC.listTyCon
    pairTy <- compileTyCon (GHC.tupleTyCon GHC.Boxed 2)
    pairConstr <- compileDataConRef (GHC.tupleDataCon GHC.Boxed 2)
    intTy <- lookupBuiltinType ''Integer
    bsTy <- lookupBuiltinType ''Builtins.ByteString
    datTy <- lookupBuiltinType ''Builtins.Data
    strTy <- lookupBuiltinType ''Builtins.String
>>>>>>> ac6d6c898 (WIP)

<<<<<<< HEAD
    defineBuiltinTerm 'Builtins.unitval $ PIR.mkConstant () ()
    defineBuiltinTerm 'Builtins.chooseUnit $ mkBuiltin PLC.ChooseUnit
||||||| parent of ac6d6c898 (WIP)
    -- See Note [Builtin terms and values] for the eta expansion below
=======
    do
        term <- builtinBoolToBool
        defineTermWithType 'builtinBoolToBool (PIR.TyFun () Bool.bool boolTy) term [bool]
    do
        term <- builtinListToListWithMap
        a <- liftQuote $ freshTyName "a"
        b <- liftQuote $ freshTyName "b"
        let ty = PIR.TyForall () a (PIR.Type ()) $
                PIR.TyForall () b (PIR.Type ()) $
                PIR.TyFun () (PIR.TyFun () (PIR.TyVar () a) (PIR.TyVar () b)) $
                PIR.TyFun ()
                  (PIR.TyApp () List.list (PIR.TyVar () a))
                  (PIR.TyApp () listTy    (PIR.TyVar () b))
        defineTermWithType 'builtinListToListWithMap ty term [lst]
    do
        term <- builtinPairToPair
        a <- liftQuote $ freshTyName "a"
        b <- liftQuote $ freshTyName "b"
        let ty = PIR.TyForall () a (PIR.Type ()) $ PIR.TyForall () b (PIR.Type ()) $
                PIR.TyFun ()
                  (PIR.TyApp () (PIR.TyApp () Pair.pair (PIR.TyVar () a)) (PIR.TyVar () b))
                  (PIR.TyApp () (PIR.TyApp () pairTy (PIR.TyVar () a)) (PIR.TyVar () b))
        defineTermWithType 'builtinPairToPair ty term [pair]

    -- See Note [Builtin terms and values] for the eta expansion below
>>>>>>> ac6d6c898 (WIP)

    -- Bytestring builtins
<<<<<<< HEAD
    defineBuiltinTerm 'Builtins.concatenate $ mkBuiltin PLC.Concatenate
    defineBuiltinTerm 'Builtins.takeByteString $ mkBuiltin PLC.TakeByteString
    defineBuiltinTerm 'Builtins.dropByteString $ mkBuiltin PLC.DropByteString
    defineBuiltinTerm 'Builtins.sha2_256 $ mkBuiltin PLC.SHA2
    defineBuiltinTerm 'Builtins.sha3_256 $ mkBuiltin PLC.SHA3
    defineBuiltinTerm 'Builtins.equalsByteString $ mkBuiltin PLC.EqByteString
    defineBuiltinTerm 'Builtins.lessThanByteString $ mkBuiltin PLC.LtByteString
    defineBuiltinTerm 'Builtins.greaterThanByteString $ mkBuiltin PLC.GtByteString
    defineBuiltinTerm 'Builtins.emptyByteString $ PIR.mkConstant () BS.empty
    defineBuiltinTerm 'Builtins.decodeUtf8 $ mkBuiltin PLC.DecodeUtf8

    -- Crypto
    defineBuiltinTerm 'Builtins.verifySignature $ mkBuiltin PLC.VerifySignature
||||||| parent of ac6d6c898 (WIP)
    do
        let term = mkBuiltin PLC.Concatenate
        defineBuiltinTerm 'Builtins.concatenate term [bs]
    do
        let term = mkBuiltin PLC.TakeByteString
        defineBuiltinTerm 'Builtins.takeByteString term [int, bs]
    do
        let term = mkBuiltin PLC.DropByteString
        defineBuiltinTerm 'Builtins.dropByteString term [int, bs]
    do
        let term = mkBuiltin PLC.SHA2
        defineBuiltinTerm 'Builtins.sha2_256 term [bs]
    do
        let term = mkBuiltin PLC.SHA3
        defineBuiltinTerm 'Builtins.sha3_256 term [bs]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.EqByteString
        defineBuiltinTerm 'Builtins.equalsByteString term [bs, bool]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.LtByteString
        defineBuiltinTerm 'Builtins.lessThanByteString term [bs, bool]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.GtByteString
        defineBuiltinTerm 'Builtins.greaterThanByteString term [bs, bool]
    do
        let term = PIR.mkConstant () BS.empty
        defineBuiltinTerm 'Builtins.emptyByteString term [bs]
    do
        let term = mkBuiltin PLC.DecodeUtf8
        defineBuiltinTerm 'Builtins.decodeUtf8 term [bs]
=======
    do
        let term = mkBuiltin PLC.Concatenate
        defineBuiltinTerm 'Builtins.concatenate term [bs]
    do
        let term = mkBuiltin PLC.TakeByteString
        defineBuiltinTerm 'Builtins.takeByteString term [int, bs]
    do
        let term = mkBuiltin PLC.DropByteString
        defineBuiltinTerm 'Builtins.dropByteString term [int, bs]
    do
        let term = mkBuiltin PLC.SHA2
        defineBuiltinTerm 'Builtins.sha2_256 term [bs]
    do
        let term = mkBuiltin PLC.SHA3
        defineBuiltinTerm 'Builtins.sha3_256 term [bs]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.EqByteString
        defineBuiltinTerm 'Builtins.equalsByteString term [bs, bool, builtinb2b]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.LtByteString
        defineBuiltinTerm 'Builtins.lessThanByteString term [bs, bool, builtinb2b]
    do
        term <- wrapRel bsTy 2 $ mkBuiltin PLC.GtByteString
        defineBuiltinTerm 'Builtins.greaterThanByteString term [bs, bool, builtinb2b]
    do
        let term = PIR.mkConstant () BS.empty
        defineBuiltinTerm 'Builtins.emptyByteString term [bs]
    do
        let term = mkBuiltin PLC.DecodeUtf8
        defineBuiltinTerm 'Builtins.decodeUtf8 term [bs]
>>>>>>> ac6d6c898 (WIP)

    -- Integer builtins
<<<<<<< HEAD
    defineBuiltinTerm 'Builtins.addInteger $ mkBuiltin PLC.AddInteger
    defineBuiltinTerm 'Builtins.subtractInteger $ mkBuiltin PLC.SubtractInteger
    defineBuiltinTerm 'Builtins.multiplyInteger $ mkBuiltin PLC.MultiplyInteger
    defineBuiltinTerm 'Builtins.divideInteger $ mkBuiltin PLC.DivideInteger
    defineBuiltinTerm 'Builtins.modInteger $ mkBuiltin PLC.ModInteger
    defineBuiltinTerm 'Builtins.quotientInteger $ mkBuiltin PLC.QuotientInteger
    defineBuiltinTerm 'Builtins.remainderInteger $ mkBuiltin PLC.RemainderInteger
    defineBuiltinTerm 'Builtins.greaterThanInteger $ mkBuiltin PLC.GreaterThanInteger
    defineBuiltinTerm 'Builtins.greaterThanEqInteger $ mkBuiltin PLC.GreaterThanEqInteger
    defineBuiltinTerm 'Builtins.lessThanInteger $ mkBuiltin PLC.LessThanInteger
    defineBuiltinTerm 'Builtins.lessThanEqInteger $ mkBuiltin PLC.LessThanEqInteger
    defineBuiltinTerm 'Builtins.equalsInteger $ mkBuiltin PLC.EqInteger
||||||| parent of ac6d6c898 (WIP)
    do
        let term = mkBuiltin PLC.AddInteger
        defineBuiltinTerm 'Builtins.addInteger term [int]
    do
        let term = mkBuiltin PLC.SubtractInteger
        defineBuiltinTerm 'Builtins.subtractInteger term [int]
    do
        let term = mkBuiltin PLC.MultiplyInteger
        defineBuiltinTerm 'Builtins.multiplyInteger term [int]
    do
        let term = mkBuiltin PLC.DivideInteger
        defineBuiltinTerm 'Builtins.divideInteger term [int]
    do
        let term = mkBuiltin PLC.ModInteger
        defineBuiltinTerm 'Builtins.modInteger term [int]
    do
        let term = mkBuiltin PLC.QuotientInteger
        defineBuiltinTerm 'Builtins.quotientInteger term [int]
    do
        let term = mkBuiltin PLC.RemainderInteger
        defineBuiltinTerm 'Builtins.remainderInteger term [int]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.GreaterThanInteger
        defineBuiltinTerm 'Builtins.greaterThanInteger term [int, bool]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.GreaterThanEqInteger
        defineBuiltinTerm 'Builtins.greaterThanEqInteger term [int, bool]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.LessThanInteger
        defineBuiltinTerm 'Builtins.lessThanInteger term [int, bool]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.LessThanEqInteger
        defineBuiltinTerm 'Builtins.lessThanEqInteger term [int, bool]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.EqInteger
        defineBuiltinTerm 'Builtins.equalsInteger term [int, bool]

    -- Blockchain builtins
    do
        term <- wrapRel bsTy 3 $ mkBuiltin PLC.VerifySignature
        defineBuiltinTerm 'Builtins.verifySignature term [bs, bool]
=======
    do
        let term = mkBuiltin PLC.AddInteger
        defineBuiltinTerm 'Builtins.addInteger term [int]
    do
        let term = mkBuiltin PLC.SubtractInteger
        defineBuiltinTerm 'Builtins.subtractInteger term [int]
    do
        let term = mkBuiltin PLC.MultiplyInteger
        defineBuiltinTerm 'Builtins.multiplyInteger term [int]
    do
        let term = mkBuiltin PLC.DivideInteger
        defineBuiltinTerm 'Builtins.divideInteger term [int]
    do
        let term = mkBuiltin PLC.ModInteger
        defineBuiltinTerm 'Builtins.modInteger term [int]
    do
        let term = mkBuiltin PLC.QuotientInteger
        defineBuiltinTerm 'Builtins.quotientInteger term [int]
    do
        let term = mkBuiltin PLC.RemainderInteger
        defineBuiltinTerm 'Builtins.remainderInteger term [int]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.GreaterThanInteger
        defineBuiltinTerm 'Builtins.greaterThanInteger term [int, bool, builtinb2b]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.GreaterThanEqInteger
        defineBuiltinTerm 'Builtins.greaterThanEqInteger term [int, bool, builtinb2b]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.LessThanInteger
        defineBuiltinTerm 'Builtins.lessThanInteger term [int, bool, builtinb2b]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.LessThanEqInteger
        defineBuiltinTerm 'Builtins.lessThanEqInteger term [int, bool, builtinb2b]
    do
        term <- wrapRel intTy 2 $ mkBuiltin PLC.EqInteger
        defineBuiltinTerm 'Builtins.equalsInteger term [int, bool, builtinb2b]

    -- Data builtins
    do
        let term = mkBuiltin PLC.ChooseData
        defineBuiltinTerm 'Builtins.chooseData term [dat]
    do
        i <- liftQuote $ freshName "i"
        args <- liftQuote $ freshName "args"
        let term = PIR.LamAbs () i intTy $ PIR.LamAbs () args (PLC.TyApp () listTy datTy) $
                PIR.mkIterApp () (mkBuiltin PLC.ConstrData) [PIR.Var () i]
        --let term = mkBuiltin PLC.Constr
        defineBuiltinTerm 'Builtins.mkConstr term [dat]
    do
        let term = mkBuiltin PLC.IData
        defineBuiltinTerm 'Builtins.mkI term [dat, int]
    do
        let term = mkBuiltin PLC.BData
        defineBuiltinTerm 'Builtins.mkB term [dat, bs]
    do
        d <- liftQuote $ freshName "d"
        x <- liftQuote $ freshName "x"
        l2l <- lookupBuiltinTerm 'builtinListToListWithMap
        let ucd = PIR.Apply () (mkBuiltin PLC.UnConstrData) (PIR.Var () d)
        let idfun = PIR.LamAbs () x datTy (PIR.Var () x)
        let pconstrInst = PIR.mkIterInst () pairConstr [intTy, PIR.TyApp () listTy datTy]
        let builtinListTy = List.list

        let componentTypes = [intTy, PIR.TyApp () builtinListTy datTy]
        let fstt = PIR.mkIterInst () (PIR.Builtin () PLC.FstPair) componentTypes
        let sndd = PIR.mkIterInst () (PIR.Builtin () PLC.SndPair) componentTypes

        let term = PIR.LamAbs () d datTy $
                PIR.mkIterApp () pconstrInst [
                  PIR.Apply () fstt ucd
                  , PIR.mkIterApp () (PIR.mkIterInst () l2l [datTy, datTy]) [idfun, PIR.Apply () sndd ucd]
                  ]

        defineBuiltinTerm 'Builtins.unsafeDataAsConstr term [dat, builtinl2l]
    do
        d <- liftQuote $ freshName "d"
        p2p <- lookupBuiltinTerm 'builtinPairToPair
        l2l <- lookupBuiltinTerm 'builtinListToListWithMap
        let bpd = PIR.mkIterTyApp () Pair.pair [datTy, datTy]
            hpd = PIR.mkIterTyApp () pairTy [datTy, datTy]
            term = PIR.LamAbs () d datTy $
              PIR.mkIterApp () (PIR.mkIterInst () l2l [bpd, hpd]) [
                  PIR.mkIterInst () p2p [datTy, datTy]
                  , PIR.Apply () (mkBuiltin PLC.UnMapData) (PIR.Var () d)
                  ]
        defineBuiltinTerm 'Builtins.unsafeDataAsMap term [dat, builtinp2p, builtinl2l]
    do
        d <- liftQuote $ freshName "d"
        x <- liftQuote $ freshName "x"
        l2l <- lookupBuiltinTerm 'builtinListToListWithMap
        let idfun =PIR.LamAbs () x datTy (PIR.Var () x)
            term = PIR.LamAbs () d datTy $
                PIR.mkIterApp () (PIR.mkIterInst () l2l [datTy, datTy])
                [idfun, PIR.Apply () (mkBuiltin PLC.UnListData) (PIR.Var () d)]
        defineBuiltinTerm 'Builtins.unsafeDataAsList term [dat, lst, builtinl2l]
    do
        let term = mkBuiltin PLC.UnIData
        defineBuiltinTerm 'Builtins.unsafeDataAsI term [dat]
    do
        let term = mkBuiltin PLC.UnBData
        defineBuiltinTerm 'Builtins.unsafeDataAsB term [dat]
    do
        term <- wrapRel datTy 2 $ mkBuiltin PLC.EqualsData
        defineBuiltinTerm 'Builtins.equalsData term [dat, bool, builtinb2b]

    -- Blockchain builtins
    do
        term <- wrapRel bsTy 3 $ mkBuiltin PLC.VerifySignature
        defineBuiltinTerm 'Builtins.verifySignature term [bs, bool, builtinb2b]
>>>>>>> ac6d6c898 (WIP)

    -- Error
    -- See Note [Delaying error]
    func <- delayedErrorFunc
    defineBuiltinTerm 'Builtins.error func

    -- Strings and chars
<<<<<<< HEAD
    defineBuiltinTerm 'Builtins.appendString $ mkBuiltin PLC.Append
    defineBuiltinTerm 'Builtins.emptyString $ PIR.mkConstant () ("" :: String)
    defineBuiltinTerm 'Builtins.charToString $ mkBuiltin PLC.CharToString
    defineBuiltinTerm 'Builtins.equalsString $ mkBuiltin PLC.EqualsString
    defineBuiltinTerm 'Builtins.trace $ mkBuiltin PLC.Trace
    defineBuiltinTerm 'Builtins.encodeUtf8 $ mkBuiltin PLC.EncodeUtf8

    -- Pairs
    defineBuiltinTerm 'Builtins.fst $ mkBuiltin PLC.FstPair
    defineBuiltinTerm 'Builtins.snd $ mkBuiltin PLC.SndPair

    -- List
    defineBuiltinTerm 'Builtins.null $ mkBuiltin PLC.NullList
    defineBuiltinTerm 'Builtins.head $ mkBuiltin PLC.HeadList
    defineBuiltinTerm 'Builtins.tail $ mkBuiltin PLC.TailList
||||||| parent of ac6d6c898 (WIP)
    do
        let term = mkBuiltin PLC.Append
        defineBuiltinTerm 'Builtins.appendString term [str]
    do
        let term = PIR.mkConstant () ("" :: String)
        defineBuiltinTerm 'Builtins.emptyString term [str]
    do
        let term = mkBuiltin PLC.CharToString
        defineBuiltinTerm 'Builtins.charToString term [char, str]
    do
        term <- wrapRel strTy 2 $ mkBuiltin PLC.EqualsString
        defineBuiltinTerm 'Builtins.equalsString term [str, bool]
    do
        term <- wrapUnitFun strTy $ mkBuiltin PLC.Trace
        defineBuiltinTerm 'Builtins.trace term [str, unit]
    do
        let term = mkBuiltin PLC.EncodeUtf8
        defineBuiltinTerm 'Builtins.encodeUtf8 term [bs]
=======
    do
        let term = mkBuiltin PLC.Append
        defineBuiltinTerm 'Builtins.appendString term [str]
    do
        let term = PIR.mkConstant () ("" :: String)
        defineBuiltinTerm 'Builtins.emptyString term [str]
    do
        let term = mkBuiltin PLC.CharToString
        defineBuiltinTerm 'Builtins.charToString term [char, str]
    do
        term <- wrapRel strTy 2 $ mkBuiltin PLC.EqualsString
        defineBuiltinTerm 'Builtins.equalsString term [str, bool, builtinb2b]
    do
        term <- wrapUnitFun strTy $ mkBuiltin PLC.Trace
        defineBuiltinTerm 'Builtins.trace term [str, unit]
    do
        let term = mkBuiltin PLC.EncodeUtf8
        defineBuiltinTerm 'Builtins.encodeUtf8 term [bs]
>>>>>>> ac6d6c898 (WIP)

defineBuiltinTypes
    :: CompilingDefault uni fun m
    => m ()
defineBuiltinTypes = do
<<<<<<< HEAD
    defineBuiltinType ''BS.ByteString $ PLC.toTypeAst $ Proxy @BS.ByteString
    defineBuiltinType ''Integer $ PLC.toTypeAst $ Proxy @Integer
    defineBuiltinType ''Builtins.BuiltinBool $ PLC.toTypeAst $ Proxy @Bool
    defineBuiltinType ''Builtins.BuiltinUnit $ PLC.toTypeAst $ Proxy @()
    defineBuiltinType ''Builtins.BuiltinString $ PLC.toTypeAst $ Proxy @String
    defineBuiltinType ''Char $ PLC.toTypeAst $ Proxy @Char
    defineBuiltinType ''Builtins.BuiltinPair $ PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoPair)
    defineBuiltinType ''Builtins.BuiltinList $ PLC.TyBuiltin () (PLC.SomeTypeIn PLC.DefaultUniProtoList)
||||||| parent of ac6d6c898 (WIP)
    do
        let ty = PLC.toTypeAst $ Proxy @BS.ByteString
        defineBuiltinType ''Builtins.ByteString ty []
    do
        let ty = PLC.toTypeAst $ Proxy @Integer
        defineBuiltinType ''Integer ty []

    -- Strings and chars
    do
        let ty = PLC.toTypeAst $ Proxy @String
        defineBuiltinType ''Builtins.String ty []
    do
        let ty = PLC.toTypeAst $ Proxy @Char
        defineBuiltinType ''Char ty []
=======
    do
        let ty = PLC.toTypeAst $ Proxy @BS.ByteString
        defineBuiltinType ''Builtins.ByteString ty []
    do
        let ty = PLC.toTypeAst $ Proxy @Integer
        defineBuiltinType ''Integer ty []

    do
        let ty = PLC.toTypeAst $ Proxy @PLC.Data
        defineBuiltinType ''Builtins.Data ty []

    -- Strings and chars
    do
        let ty = PLC.toTypeAst $ Proxy @String
        defineBuiltinType ''Builtins.String ty []
    do
        let ty = PLC.toTypeAst $ Proxy @Char
        defineBuiltinType ''Char ty []
>>>>>>> ac6d6c898 (WIP)

-- | Lookup a builtin term by its TH name. These are assumed to be present, so fails if it cannot find it.
lookupBuiltinTerm :: Compiling uni fun m => TH.Name -> m (PIRTerm uni fun)
lookupBuiltinTerm name = do
    ghcName <- GHC.getName <$> getThing name
    maybeTerm <- PIR.lookupTerm () (LexName ghcName)
    case maybeTerm of
        Just t  -> pure t
        Nothing -> throwSd CompilationError $ "Missing builtin definition:" GHC.<+> (GHC.text $ show name)

-- | Lookup a builtin type by its TH name. These are assumed to be present, so fails if it is cannot find it.
lookupBuiltinType :: Compiling uni fun m => TH.Name -> m (PIRType uni)
lookupBuiltinType name = do
    ghcName <- GHC.getName <$> getThing name
    maybeType <- PIR.lookupType () (LexName ghcName)
    case maybeType of
        Just t  -> pure t
        Nothing -> throwSd CompilationError $ "Missing builtin definition:" GHC.<+> (GHC.text $ show name)

-- | The function 'error :: forall a . a'.
errorFunc :: Compiling uni fun m => m (PIRTerm uni fun)
errorFunc = do
    n <- safeFreshTyName "e"
    pure $ PIR.TyAbs () n (PIR.Type ()) (PIR.Error () (PIR.TyVar () n))

-- | The delayed error function 'error :: forall a . () -> a'.
delayedErrorFunc :: CompilingDefault uni fun m => m (PIRTerm uni fun)
delayedErrorFunc = do
    n <- safeFreshTyName "a"
    t <- liftQuote (freshName "thunk")
    let ty = PLC.toTypeAst $ Proxy @()
    pure $ PIR.TyAbs () n (PIR.Type ()) $ PIR.LamAbs () t ty $ PIR.Error () (PIR.TyVar () n)

-- | The type 'forall a. a'.
errorTy :: Compiling uni fun m => m (PIRType uni)
errorTy = do
    tyname <- safeFreshTyName "a"
    pure $ PIR.TyForall () tyname (PIR.Type ()) (PIR.TyVar () tyname)
<<<<<<< HEAD
||||||| parent of ac6d6c898 (WIP)

-- TODO: bind the converter to a name too. Need an appropriate GHC.Name for
-- it, since that's what our definitions are hung off. Also the type wouldn't
-- be a simple conversion of the Haskell type, because it takes a Scott boolean.
-- | Convert a Scott-encoded Boolean into a Haskell Boolean.
scottBoolToHaskellBool :: CompilingDefault uni fun m => m (PIRTerm uni fun)
scottBoolToHaskellBool = do
    let scottBoolTy = Bool.bool
    haskellBoolTy <- compileType GHC.boolTy

    arg <- liftQuote $ freshName "b"
    let instantiatedMatch = PIR.TyInst () (PIR.Builtin () PLC.IfThenElse) haskellBoolTy

    haskellTrue <- compileDataConRef GHC.trueDataCon
    haskellFalse <- compileDataConRef GHC.falseDataCon
    pure $
        PIR.LamAbs () arg scottBoolTy $
        PIR.mkIterApp () instantiatedMatch [ (PIR.Var () arg), haskellTrue, haskellFalse ]

-- | Wrap an relation of arity @n@ that produces a Scott boolean.
wrapRel :: CompilingDefault uni fun m => PIRType uni -> Int -> PIRTerm uni fun -> m (PIRTerm uni fun)
wrapRel argTy arity term = do
    args <- replicateM arity $ do
        name <- safeFreshName "arg"
        pure $ PIR.VarDecl () name argTy

    converter <- scottBoolToHaskellBool

    pure $
        PIR.mkIterLamAbs args $
        PIR.Apply () converter (PIR.mkIterApp () term (fmap (PIR.mkVar ()) args))

-- | Convert a Scott-encoded Unit into a Haskell Unit.
scottUnitToHaskellUnit :: CompilingDefault uni fun m => m (PIRTerm uni fun)
scottUnitToHaskellUnit = do
    let scottUnitTy = Unit.unit

    arg <- liftQuote $ freshName "b"

    haskellUnitVal <- compileDataConRef GHC.unitDataCon
    pure $ PIR.LamAbs () arg scottUnitTy haskellUnitVal

-- | Wrap an function with the given argument type that produces a Scott unit.
wrapUnitFun :: CompilingDefault uni fun m => PIRType uni -> PIRTerm uni fun -> m (PIRTerm uni fun)
wrapUnitFun argTy term = do
    arg <- do
        name <- safeFreshName "arg"
        pure $ PIR.VarDecl () name argTy

    converter <- scottUnitToHaskellUnit

    pure $
        PIR.mkIterLamAbs [arg] $
        PIR.Apply () converter (PIR.Apply () term (PIR.mkVar () arg))
=======

-- TODO: bind the converter to a name too. Need an appropriate GHC.Name for
-- it, since that's what our definitions are hung off. Also the type wouldn't
-- be a simple conversion of the Haskell type, because it takes a Scott boolean.
-- | Convert a builtin Boolean into a Haskell Boolean.
builtinBoolToBool :: CompilingDefault uni fun m => m (PIRTerm uni fun)
builtinBoolToBool = do
    let builtinBoolTy = Bool.bool
    haskellBoolTy <- compileType GHC.boolTy

    arg <- liftQuote $ freshName "b"
    let instantiatedMatch = PIR.TyInst () (PIR.Builtin () PLC.IfThenElse) haskellBoolTy

    haskellTrue <- compileDataConRef GHC.trueDataCon
    haskellFalse <- compileDataConRef GHC.falseDataCon
    pure $
        PIR.LamAbs () arg builtinBoolTy $
        PIR.mkIterApp () instantiatedMatch [ (PIR.Var () arg), haskellTrue, haskellFalse ]

-- | Wrap an relation of arity @n@ that produces a builtin boolean.
wrapRel :: CompilingDefault uni fun m => PIRType uni -> Int -> PIRTerm uni fun -> m (PIRTerm uni fun)
wrapRel argTy arity term = do
    args <- replicateM arity $ do
        name <- safeFreshName "arg"
        pure $ PIR.VarDecl () name argTy

    converter <- lookupBuiltinTerm 'builtinBoolToBool

    pure $
        PIR.mkIterLamAbs args $
        PIR.Apply () converter (PIR.mkIterApp () term (fmap (PIR.mkVar ()) args))

-- | Convert a builtin Unit into a Haskell Unit.
builtinUnitToHaskellUnit :: CompilingDefault uni fun m => m (PIRTerm uni fun)
builtinUnitToHaskellUnit = do
    let builtinUnitTy = Unit.unit

    arg <- liftQuote $ freshName "b"

    haskellUnitVal <- compileDataConRef GHC.unitDataCon
    pure $ PIR.LamAbs () arg builtinUnitTy haskellUnitVal

-- | Wrap an function with the given argument type that produces a builtin unit.
wrapUnitFun :: CompilingDefault uni fun m => PIRType uni -> PIRTerm uni fun -> m (PIRTerm uni fun)
wrapUnitFun argTy term = do
    arg <- do
        name <- safeFreshName "arg"
        pure $ PIR.VarDecl () name argTy

    converter <- builtinUnitToHaskellUnit

    pure $
        PIR.mkIterLamAbs [arg] $
        PIR.Apply () converter (PIR.Apply () term (PIR.mkVar () arg))

builtinPairToPair :: CompilingDefault uni fun m => m (PIRTerm uni fun)
builtinPairToPair = do
    aty <- liftQuote $ freshTyName "a"
    bty <- liftQuote $ freshTyName "b"
    let ab = [ PIR.TyVar () aty, PIR.TyVar () bty ]

    -- pair
    let builtinPairTy = Pair.pair
    -- [pair a b]
    let builtinPairTyApp = PIR.mkIterTyApp () builtinPairTy ab
    -- (,) (the type constructor)
    haskellPairTy <- compileTyCon (GHC.tupleTyCon GHC.Boxed 2)
    -- [(,) a b]
    let haskellPairTyApp = PIR.mkIterTyApp () haskellPairTy ab
    -- (,) (the data constructor)
    haskellPairConstr <- compileDataConRef (GHC.tupleDataCon GHC.Boxed 2)
    -- { (,) a b}
    let haskellPairConstrInst = PIR.mkIterInst () haskellPairConstr ab


    let uncur = Pair.uncurry
    -- Type arguments are first the two types of the pair components, and then
    -- the result type.
    let instantiatedUncur = PIR.mkIterInst () uncur (ab ++ [ haskellPairTyApp ])

    arg <- liftQuote $ freshName "p"
    pure $
        PIR.TyAbs () aty (PLC.Type ()) $
        PIR.TyAbs () bty (PLC.Type ()) $
        PIR.LamAbs () arg builtinPairTyApp $
        PIR.mkIterApp () instantiatedUncur [haskellPairConstrInst, PIR.Var () arg]

builtinListToListWithMap :: CompilingDefault uni fun m => m (PIRTerm uni fun)
builtinListToListWithMap = do
    aty <- liftQuote $ freshTyName "a"
    bty <- liftQuote $ freshTyName "b"
    let a = PIR.TyVar () aty
    let b = PIR.TyVar () bty

    -- list
    let builtinListTy = List.list
    -- [list a]
    let builtinListTyApp = PIR.TyApp () builtinListTy a
    -- [] (the type constructor)
    haskellListTy <- compileTyCon GHC.listTyCon
    -- [[] a]
    let haskellListTyApp = PIR.TyApp () haskellListTy b
    -- (:) (the data constructor)
    haskellConsConstr <- compileDataConRef GHC.consDataCon
    -- { (:} a }
    let haskellConsConstrInst = PIR.TyInst () haskellConsConstr b
    -- [] (the data constructor)
    haskellNilConstr <- compileDataConRef GHC.nilDataCon
    -- { [] a }
    let haskellNilConstrInst = PIR.TyInst () haskellNilConstr b

    let foldrList = List.foldrList
    -- Type arguments are first types of the list contents, and then the result type.
    let instantiatedFold = PIR.mkIterInst () foldrList [a , haskellListTyApp]

    farg <- liftQuote $ freshName "f"
    larg <- liftQuote $ freshName "l"

    folder <- do
      e <- liftQuote $ freshName "a"
      r <- liftQuote $ freshName "r"
      pure $ PIR.mkIterLamAbs [PIR.VarDecl () e (PIR.TyVar () aty), PIR.VarDecl () r haskellListTyApp] $
          PIR.mkIterApp () haskellConsConstrInst [PIR.Apply () (PIR.Var () farg) (PIR.Var () e), PIR.Var () r]

    pure $
        PIR.TyAbs () aty (PLC.Type ()) $
        PIR.TyAbs () bty (PLC.Type ()) $
        PIR.LamAbs () farg (PLC.TyFun () (PIR.TyVar () aty) (PIR.TyVar () bty)) $
        PIR.LamAbs () larg builtinListTyApp $
        PIR.mkIterApp () instantiatedFold [folder, haskellNilConstrInst, PIR.Var () larg]
>>>>>>> ac6d6c898 (WIP)
