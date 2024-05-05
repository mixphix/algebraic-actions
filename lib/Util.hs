{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import GHC.TypeLits (KnownNat, type (+), type (*))
import GHC.TypeLits.Witnesses
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Group

onVector :: ([x] -> [x]) -> Vector x -> Vector x
onVector f = Vector.fromList . f . Vector.toList

with2n :: forall n r. ((KnownNat (n + n)) => r) -> ((KnownNat n) => r)
with2n r = case SNat @n %+ SNat @n of SNat -> r

withAdd ::
  forall m n r. ((KnownNat (m + n)) => r) -> ((KnownNat m, KnownNat n) => r)
withAdd r = case SNat @m %+ SNat @n of SNat -> r

withMul ::
  forall m n r. ((KnownNat (m * n)) => r) -> ((KnownNat m, KnownNat n) => r)
withMul r = case SNat @m %* SNat @n of SNat -> r

nat :: forall n i r. (KnownNat n, Integral i) => (i -> r) -> ((KnownNat n) => r)
nat f = f . fromIntegral $ fromSNat (SNat @n)

(¢) :: (Group g) => g -> g -> g
g ¢ h = invert g <> invert h <> g <> h
