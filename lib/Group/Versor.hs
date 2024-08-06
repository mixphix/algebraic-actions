{-# OPTIONS_GHC -Wno-orphans #-}

-- | SO(3)
module Group.Versor where

import Control.Monad (guard)
import Data.Action
import Data.Functor (($>))
import Data.Group (Group (invert))
import Group.Quaternion
import Linear.Metric (quadrance)
import Linear.V3
import Linear.V4
import Linear.Vector ((^*))

newtype Versor n = Versor (V4 n)
  deriving (Eq, Show, Functor, Foldable, Traversable)
instance (Num n) => Semigroup (Versor n) where
  (<>) :: (Num n) => Versor n -> Versor n -> Versor n
  -- Just so damn cool.
  Versor (V4 a1 b1 c1 d1) <> Versor (V4 a2 b2 c2 d2) = Versor $ V4
    do a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
    do a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
    do a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
    do a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
instance (Num n) => Monoid (Versor n) where
  mempty :: (Num n) => Versor n
  mempty = Versor (V4 1 0 0 0)
instance (Fractional n) => Group (Versor n) where
  invert :: (Fractional n) => Versor n -> Versor n
  invert g@(Versor v) =
    let Versor v' = conjugate g
     in Versor (v' ^* recip (quadrance v))

conjugate :: (Num n) => Versor n -> Versor n
conjugate (Versor (V4 a i j k)) = Versor (V4 a (-i) (-j) (-k))

qVersor :: (Num n) => Quaternion -> Versor n
qVersor q = Versor case q of
  Qe -> V4 1 0 0 0
  Qi -> V4 0 1 0 0
  Qe' -> V4 (-1) 0 0 0
  Qi' -> V4 0 (-1) 0 0
  Qj -> V4 0 0 1 0
  Qj' -> V4 0 0 (-1) 0
  Qk -> V4 0 0 0 1
  Qk' -> V4 0 0 0 (-1)

versor :: (Num n, Eq n) => V4 n -> Maybe (Versor n)
versor v = guard (quadrance v == 1) $> Versor v

instance (Fractional n) => Action (Versor n) (V3 n) where
  action :: (Fractional n) => Versor n -> V3 n -> V3 n
  action v (V3 x y z) =
    let Versor (V4 _ x' y' z') = v <> Versor (V4 0 x y z) <> invert v
     in V3 x' y' z'

instance (Fractional n) => Action Quaternion (V3 n) where
  action :: (Fractional n) => Quaternion -> V3 n -> V3 n
  action = action . qVersor @n
