module Group.Permute where

import Control.Block
import Control.Monad
import Data.Functor (($>))
import Data.Group
import Data.Set qualified as Set
import Data.Vector (Vector, generate, (!))
import Data.Vector qualified as Vector
import GHC.TypeLits
import Linear.V
import Util

newtype Permute n = MkPermute (V n Int) deriving (Eq, Ord, Show)
pattern Permute :: (KnownNat n) => Vector Int -> Permute n
pattern Permute p = MkPermute (V p)

{-# COMPLETE Permute #-}

instance (KnownNat n) => Semigroup (Permute n) where
  Permute v <> Permute w = Permute $ nat @n generate do (w !) . (v !)
instance (KnownNat n) => Monoid (Permute n) where
  mempty = Permute $ nat @n generate id
instance (KnownNat n) => Group (Permute n) where
  invert (Permute v) = Permute $ Vector.fromList do
    snd . unzip . Set.toList $ ireduce v \ix p -> Set.singleton (p, ix)

permute :: forall n. (KnownNat n) => V n Int -> Maybe (Permute n)
permute (V v) = nat @n \n -> do
  guard (foldMap Set.singleton v == [0 .. n - 1]) $> Permute v

permute' :: forall n. (KnownNat n) => Vector Int -> Maybe (Permute n)
permute' v = nat @n \n -> do guard (length v == n) *> permute (V v)

newtype Alternate n = MkAlternate (V n Int) deriving (Eq, Ord, Show)
pattern Alternate :: (KnownNat n) => Vector Int -> Alternate n
pattern Alternate p = MkAlternate (V p)

alternate :: forall n. (KnownNat n) => V n Int -> Maybe (Alternate n)
alternate v = case permute v of p -> guard (p <> p == mempty) $> MkAlternate v

alternate' :: forall n. (KnownNat n) => Vector Int -> Maybe (Alternate n)
alternate' v = nat @n \n -> do guard (length v == n) *> alternate (V v)

deriving via Permute (n :: Nat) instance (KnownNat n) => Semigroup (Alternate n)
deriving via Permute (n :: Nat) instance (KnownNat n) => Monoid (Alternate n)
deriving via Permute (n :: Nat) instance (KnownNat n) => Group (Alternate n)
