module Group.Linear where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Group
import GHC.TypeNats
import Linear.Matrix
import Linear.V

newtype SL n x = SL (V n (V n x))
  deriving (Eq, Show, Functor, Foldable, Traversable)
instance (KnownNat n, Num x) => Semigroup (SL n x) where
  SL m0 <> SL m1 = SL (m0 !*! m1)
instance (KnownNat n, Num x) => Monoid (SL n x) where mempty = SL 0
instance (KnownNat n, Fractional x) => Group (SL n x) where
  invert (SL mat) = SL (luInvFinite mat)

sl :: (KnownNat n, Fractional x, Eq x) => V n (V n x) -> Maybe (SL n x)
sl mat = guard (abs (luDet mat) == 1) $> SL mat

newtype O n x = O (V n (V n x))
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriving via SL (n :: Nat) x instance (KnownNat n, Num x) => Semigroup (O n x)
deriving via SL (n :: Nat) x instance (KnownNat n, Num x) => Monoid (O n x)
deriving via
  SL (n :: Nat) x
  instance
    (KnownNat n, Fractional x) => Group (O n x)

ortho :: (KnownNat n, Eq x, Fractional x) => V n (V n x) -> Maybe (O n x)
ortho mat = guard (transpose mat == luInvFinite mat) $> O mat
