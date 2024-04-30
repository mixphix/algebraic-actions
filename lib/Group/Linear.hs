module Group.Linear where

import Data.Group
import GHC.TypeNats
import Linear.Matrix
import Linear.V

newtype SL n x = SL (V n (V n x))
  deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (KnownNat n, Monoid x, Fractional x) => Group (SL n x) where
  invert :: (KnownNat n, Monoid x, Fractional x) => SL n x -> SL n x
  invert (SL mat) = SL (luInvFinite mat)

sl :: (KnownNat n, Fractional x, Eq x) => V n (V n x) -> Maybe (SL n x)
sl mat = case abs (luDet mat) of
  1 -> Just (SL mat)
  _ -> Nothing
