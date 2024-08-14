module Data.Action
  ( Action (action)
  , actions
  ) where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Semigroup

-- | One type can affect, or perform an 'action', on another in some way.
class Action g x where
  action :: g -> (x -> x)

-- | Semigroups act on themselves.
instance (Semigroup g) => Action g g where
  action :: g -> (g -> g)
  action = (<>)

-- | Elements act on linked lists by prepending.
instance Action x [x] where
  action :: x -> ([x] -> [x])
  action = (:)

-- | Elements act on linked lists by prepending.
instance Action x (NonEmpty x) where
  action :: x -> (NonEmpty x -> NonEmpty x)
  action = (<|)

-- | All of the actions in a 'Foldable' container can constitute one action.
actions :: (Foldable t, Action g x) => t g -> (x -> x)
actions = foldl' (\a -> (.) a . action) id

-- | Input-valued functions act on their inputs.
instance Action (x -> x) x where
  action :: (x -> x) -> (x -> x)
  action = ($)

-- | Endomorphisms act on their inputs.
instance Action (Endo x) x where
  action :: Endo x -> (x -> x)
  action = appEndo

-- | Actions act on functions via precomposition.
instance (Action h ø) => Action h (ø -> a) where
  action :: (Action h ø) => h -> ((ø -> a) -> (ø -> a))
  action h = (. action h)

-- Actions of various semigroups on their base type.

instance Action (First x) x where
  action :: First x -> x -> x
  action (First f) = const f

instance Action (Last x) x where
  action :: Last x -> x -> x
  action (Last _) = id

instance (Ord x) => Action (Min x) x where
  action :: Min x -> x -> x
  action (Min x) = min x

instance (Ord x) => Action (Max x) x where
  action :: Max x -> x -> x
  action (Max x) = max x

instance (Num n) => Action (Sum n) n where
  action :: Sum n -> n -> n
  action (Sum x) y = x + y

instance (Num n) => Action (Product n) n where
  action :: Product n -> n -> n
  action (Product x) y = x * y
