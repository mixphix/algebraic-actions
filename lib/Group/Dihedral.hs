{-# LANGUAGE ViewPatterns #-}

module Group.Dihedral where

import Control.Monad
import Data.Action
import Data.Complex
import Data.Functor (($>))
import Data.List (transpose)
import GHC.TypeLits (KnownNat)
import GHC.TypeLits.Witnesses
import Linear (V2 (..))
import Util

newtype Dihedral n = Dihedral Natural
  deriving (Eq, Show)
instance (KnownNat n) => Semigroup (Dihedral n) where
  (<>) :: (KnownNat n) => Dihedral n -> Dihedral n -> Dihedral n
  (<>) = \cases
    (R i) (R j) -> R (i + j)
    (R i) (S j) -> S (i + j)
    (S i) (R j) -> S (n + i - j)
    (S i) (S j) -> R (n + i - j)
   where
    n = fromSNat (SNat @n)

dihedral :: forall n x. (KnownNat n, Integral x) => x -> Dihedral n
dihedral i = nat @n \n -> Dihedral $ fromIntegral (mod i (n * 2))

r :: forall n. (KnownNat n) => Dihedral n -> Maybe Natural
r (Dihedral n) = guard (even n) $> fromIntegral (n `div` 2)

s :: forall n. (KnownNat n) => Dihedral n -> Maybe Natural
s (Dihedral n) = guard (odd n) $> fromIntegral (pred n `div` 2)

pattern R :: (KnownNat n) => Natural -> Dihedral n
pattern R n <- (r -> Just n)
  where
    R n = dihedral (2 * n)

pattern S :: (KnownNat n) => Natural -> Dihedral n
pattern S n <- (s -> Just n)
  where
    S n = dihedral (2 * n + 1)

{-# COMPLETE R, S #-}

instance Action (Dihedral 4) [[x]] where
  action :: Dihedral 4 -> [[x]] -> [[x]]
  action = \case
    R 0 -> id
    R 1 -> reverse . transpose
    R 2 -> reverse . fmap reverse
    R _ -> transpose . reverse
    S 0 -> fmap reverse
    S 1 -> transpose
    S 2 -> reverse
    S _ -> reverse . transpose . reverse

instance (Num x) => Action (Dihedral 4) (V2 x) where
  action :: Dihedral 4 -> V2 x -> V2 x
  action = \case
    R 0 -> id
    R 1 -> \(V2 x y) -> V2 (-y) x
    R 2 -> \(V2 x y) -> V2 (-x) (-y)
    R _ -> \(V2 x y) -> V2 y (-x)
    S 0 -> \(V2 x y) -> V2 (-x) y
    S 1 -> \(V2 x y) -> V2 (-y) (-x)
    S 2 -> \(V2 x y) -> V2 x (-y)
    S _ -> \(V2 x y) -> V2 y x

instance (Num x) => Action (Dihedral 4) (Complex x) where
  action :: Dihedral 4 -> Complex x -> Complex x
  action = \case
    R 0 -> id
    R 1 -> \(x :+ y) -> (-y) :+ x
    R 2 -> \(x :+ y) -> (-x) :+ (-y)
    R _ -> \(x :+ y) -> y :+ (-x)
    S 0 -> \(x :+ y) -> (-x) :+ y
    S 1 -> \(x :+ y) -> (-y) :+ (-x)
    S 2 -> \(x :+ y) -> x :+ (-y)
    S _ -> \(x :+ y) -> y :+ x
