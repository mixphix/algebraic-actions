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
instance (KnownNat n, Monoid x, Fractional x) => Group (SL n x) where
  invert (SL mat) = SL (luInvFinite mat)

sl :: (KnownNat n, Fractional x, Eq x) => V n (V n x) -> Maybe (SL n x)
sl mat = case abs (luDet mat) of
  1 -> Just (SL mat)
  _ -> Nothing

newtype O n x = O (SL n x) deriving (Eq, Show, Functor, Foldable, Traversable)
instance (KnownNat n, Num x) => Semigroup (O n x) where
  O o1 <> O o2 = O (o1 <> o2)
instance (KnownNat n, Num x) => Monoid (O n x) where mempty = O (SL 0)
instance (KnownNat n, Monoid x, Fractional x) => Group (O n x) where
  invert (O mat) = O (invert mat)

ortho :: (KnownNat n, Monoid x, Eq x, Fractional x) => SL n x -> Maybe (O n x)
ortho mat@(SL v) = guard (SL (transpose v) == invert mat) $> O (SL v)
