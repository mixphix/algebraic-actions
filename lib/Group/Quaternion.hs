module Group.Quaternion where

import Data.Group

data Quaternion
  = Qe
  | Qi
  | Qe'
  | Qi'
  | Qj
  | Qj'
  | Qk
  | Qk'
instance Semigroup Quaternion where
  (<>) :: Quaternion -> Quaternion -> Quaternion
  (<>) = \cases
    Qe x -> x
    x Qe -> x
    Qi Qi -> Qe'
    Qi Qj -> Qk
    Qi Qk -> Qj'
    Qj Qi -> Qk'
    Qj Qj -> Qe'
    Qj Qk -> Qi
    Qk Qi -> Qj
    Qk Qj -> Qi'
    Qk Qk -> Qe'
    q' x -> x <> invert q'
instance Monoid Quaternion where
  mempty :: Quaternion
  mempty = Qe
instance Group Quaternion where
  invert :: Quaternion -> Quaternion
  invert = \case
    Qe -> Qe'
    Qi -> Qi'
    Qe' -> Qe
    Qi' -> Qi
    Qj -> Qj'
    Qj' -> Qj
    Qk -> Qk'
    Qk' -> Qk
