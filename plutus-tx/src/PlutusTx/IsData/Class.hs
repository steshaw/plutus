{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
module PlutusTx.IsData.Class where

import           Data.ByteString      as BS

import           Prelude              (Int, Integer, Maybe (..), error)

import qualified PlutusCore.Data      as TrueData
import           PlutusTx.Builtins

import           PlutusTx.Functor
import           PlutusTx.Traversable

import           Data.Kind
import           Data.Void

import           GHC.TypeLits         (ErrorMessage (..), TypeError)


{-# ANN module "HLint: ignore" #-}

-- | A typeclass for types that can be converted to and from 'Data'.
class IsData (a :: Type) where
    toData :: a -> BuiltinData
    -- TODO: this should probably provide some kind of diagnostics
    fromData :: BuiltinData -> Maybe a

instance IsData BuiltinData where
    {-# INLINABLE toData #-}
    toData = id
    {-# INLINABLE fromData #-}
    fromData d = Just d

instance (TypeError ('Text "Int is not supported, use Integer instead"))
    => IsData Int where
    toData = Prelude.error "unsupported"
    fromData = Prelude.error "unsupported"

instance IsData Integer where
    {-# INLINABLE toData #-}
    toData _ = Prelude.error "no" -- I i
    {-# INLINABLE fromData #-}
    fromData d = matchData d (\_ _ -> Nothing) (const Nothing) (const Nothing) (\i -> Just i) (const Nothing)

instance IsData ByteString where
    {-# INLINABLE toData #-}
    toData b = Prelude.error "no" -- B b
    {-# INLINABLE fromData #-}
    fromData d = matchData d (\_ _ -> Nothing) (const Nothing) (const Nothing) (const Nothing) (\b -> Just b)

instance IsData a => IsData [a] where
    {-# INLINABLE toData #-}
    toData xs = Prelude.error "no" -- List (fmap toData xs)
    {-# INLINABLE fromData #-}
    fromData d = matchData d (\_ _ -> Nothing) (const Nothing) (\l -> traverse fromData l) (const Nothing) (const Nothing)

instance IsData Void where
    {-# INLINABLE toData #-}
    toData v = absurd v
    {-# INLINABLE fromData #-}
    fromData _ = Nothing
