module Group.Symplectic where

import Control.Monad
import Data.Functor (($>))
import Data.Group
import Data.Vector (fromList)
import GHC.TypeLits.Witnesses
import GHC.TypeNats (KnownNat, type (+))
import Linear.Matrix
import Linear.V
import Util (with2n)

newtype Sp n x = Sp (V (n + n) (V (n + n) x))
  deriving (Functor, Foldable, Traversable)

instance (KnownNat n, Semigroup x) => Semigroup (Sp n x) where
  (<>) :: (KnownNat n) => Sp n x -> Sp n x -> Sp n x
  Sp m0 <> Sp m1 = with2n @n do Sp (m0 <> m1)
instance (KnownNat n, Monoid x) => Monoid (Sp n x) where
  mempty :: (KnownNat n, Monoid x) => Sp n x
  mempty = with2n @n do Sp mempty
instance (KnownNat n, Monoid x, Fractional x) => Group (Sp n x) where
  invert :: (KnownNat n, Monoid x, Fractional x) => Sp n x -> Sp n x
  invert (Sp mat) = with2n @n do Sp (luInvFinite mat)

omega :: forall n x. (KnownNat n, Num x) => Sp n x
omega = with2n @n do
  let n = fromSNat (SNat @n)
      ø :: Natural -> V (n + n) x
      ø i
        | i < n =
            V $ fromList [if j == i + n then 1 else 0 | j <- [0 .. 2 * n - 1]]
        | otherwise =
            V $ fromList [if j == i - n then 1 else 0 | j <- [0 .. 2 * n - 1]]
   in Sp $ V (ø <$> fromList [0 .. 2 * n - 1])

sp ::
  forall n x.
  (KnownNat n, Eq x, Num x) =>
  V (n + n) (V (n + n) x) ->
  Maybe (Sp n x)
sp v = with2n @n do
  let Sp ø = omega @n @x
  guard (transpose v !*! ø !*! v == ø) $> Sp v
