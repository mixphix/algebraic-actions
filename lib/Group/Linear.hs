module Group.Linear where

import Data.Group
import GHC.TypeNats
import Linear.Matrix
import Linear.V

newtype GL n x = GL (V n (V n x))
  deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (KnownNat n, Monoid x, Fractional x) => Group (GL n x) where
  invert :: (KnownNat n, Monoid x, Fractional x) => GL n x -> GL n x
  invert (GL mat) = GL (luInvFinite mat)

gl :: (KnownNat n, Fractional x, Eq x) => V n (V n x) -> Maybe (GL n x)
gl mat = case luDet mat of
  1 -> Just (GL mat)
  _ -> Nothing
