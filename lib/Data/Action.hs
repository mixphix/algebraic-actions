module Data.Action where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty, (<|))

class Action g x where
  action :: g -> x -> x

instance (Semigroup g) => Action g g where
  action :: (Semigroup g) => g -> g -> g
  action = (<>)

instance Action x [x] where
  action :: x -> [x] -> [x]
  action = (:)

instance Action x (NonEmpty x) where
  action :: x -> NonEmpty x -> NonEmpty x
  action = (<|)

actions :: (Foldable t, Action g x) => t g -> (x -> x)
actions = foldl' (\a -> (.) a . action) id

instance (Action h ø) => Action h (ø -> a) where
  action :: (Action h ø) => h -> (ø -> a) -> (ø -> a)
  action h f = f . action h
