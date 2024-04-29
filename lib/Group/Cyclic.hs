module Group.Cyclic where

import Data.Group (Group (..))
import Data.Kind (Type)
import GHC.TypeNats

type Cyclic :: Natural -> Type
newtype Cyclic n = Cyclic Natural
  deriving (Eq, Ord, Show)
instance (KnownNat n) => Semigroup (Cyclic n) where
  (<>) :: (KnownNat n) => Cyclic n -> Cyclic n -> Cyclic n
  Cyclic s0 <> Cyclic s1 = cyclic (s0 + s1)
instance (KnownNat n) => Monoid (Cyclic n) where
  mempty :: (KnownNat n) => Cyclic n
  mempty = Cyclic 0
instance (KnownNat n) => Group (Cyclic n) where
  invert :: (KnownNat n) => Cyclic n -> Cyclic n
  invert (Cyclic n) = cyclic (fromIntegral @_ @Integer n)

cyclic :: forall n x. (KnownNat n, Integral x) => x -> Cyclic n
cyclic i = case fromSNat (SNat @n) of
  l -> Cyclic $ fromIntegral (i `mod` fromIntegral l)
