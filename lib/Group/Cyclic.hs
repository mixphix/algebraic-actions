module Group.Cyclic where

import Data.Group (Group (..))
import GHC.TypeNats
import Util (nat)

newtype Cyclic n = Cyclic Natural deriving (Eq, Ord, Show)
instance (KnownNat n) => Semigroup (Cyclic n) where
  Cyclic s0 <> Cyclic s1 = cyclic (s0 + s1)
instance (KnownNat n) => Monoid (Cyclic n) where
  mempty = Cyclic 0
instance (KnownNat n) => Group (Cyclic n) where
  invert (Cyclic n) = cyclic (-n)

cyclic :: forall n x. (KnownNat n, Integral x) => x -> Cyclic n
cyclic x = Cyclic $ nat @n \n -> fromIntegral x `mod` n
