module Group.Linear where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Group
import GHC.TypeNats
import Linear.Matrix
import Linear.V

newtype SL n x = SL (V n (V n x))
  deriving (Eq, Semigroup, Monoid, Functor, Foldable, Traversable)
instance (KnownNat n, Monoid x, Fractional x) => Group (SL n x) where
  invert :: (KnownNat n, Monoid x, Fractional x) => SL n x -> SL n x
  invert (SL mat) = SL (luInvFinite mat)

sl :: (KnownNat n, Fractional x, Eq x) => V n (V n x) -> Maybe (SL n x)
sl mat = case abs (luDet mat) of
  1 -> Just (SL mat)
  _ -> Nothing

newtype O n x = O (SL n x)
  deriving (Eq, Semigroup, Monoid, Functor, Foldable, Traversable)
instance (KnownNat n, Monoid x, Fractional x) => Group (O n x) where
  invert :: (KnownNat n, Monoid x, Fractional x) => O n x -> O n x
  invert (O mat) = O (invert mat)

ortho :: (KnownNat n, Monoid x, Eq x, Fractional x) => SL n x -> Maybe (O n x)
ortho mat@(SL v) = guard (SL (transpose v) == invert mat) $> O (SL v)
