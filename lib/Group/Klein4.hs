module Group.Klein4 (Klein4 (..)) where

import Data.Group

data Klein4
  = KleinE
  | KleinA
  | KleinB
  | KleinC
  deriving (Eq, Show)
instance Semigroup Klein4 where
  (<>) :: Klein4 -> Klein4 -> Klein4
  (<>) = \cases
    KleinE x -> x
    x KleinE -> x
    KleinA KleinB -> KleinC
    KleinA KleinC -> KleinB
    KleinB KleinA -> KleinC
    KleinB KleinC -> KleinA
    KleinC KleinA -> KleinB
    KleinC KleinB -> KleinA
    _ _ -> KleinE
instance Monoid Klein4 where
  mempty :: Klein4
  mempty = KleinE
instance Group Klein4 where
  invert :: Klein4 -> Klein4
  invert = id
