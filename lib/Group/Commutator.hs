module Group.Commutator where

import Data.Group

newtype Commutator g = Commutator {getCommutator :: g}
  deriving newtype (Eq, Ord, Show)
instance (Group g) => Semigroup (Commutator g) where
  Commutator !g <> Commutator !h = Commutator (invert g <> invert h <> g <> h)
deriving newtype instance (Group g) => Monoid (Commutator g)
deriving newtype instance (Group g) => Group (Commutator g)
