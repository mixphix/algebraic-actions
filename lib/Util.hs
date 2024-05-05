{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import GHC.Records
import GHC.TypeLits (KnownNat, type (+), type (*))
import GHC.TypeLits.Witnesses

with2n :: forall n r. ((KnownNat (n + n)) => r) -> ((KnownNat n) => r)
with2n r = case SNat @n %+ SNat @n of SNat -> r

withAdd ::
  forall m n r. ((KnownNat (m + n)) => r) -> ((KnownNat m, KnownNat n) => r)
withAdd r = case SNat @m %+ SNat @n of SNat -> r

withMul ::
  forall m n r. ((KnownNat (m * n)) => r) -> ((KnownNat m, KnownNat n) => r)
withMul r = case SNat @m %* SNat @n of SNat -> r

instance (KnownNat n) => HasField "nat" (SNat n) Natural where
  getField = fromSNat
