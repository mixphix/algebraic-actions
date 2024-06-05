{-# LANGUAGE UndecidableInstances #-}

module Group.Free where

import Data.Function (fix)
import Data.Group
import Data.Sequence (Seq (..))
import GHC.IsList

data G ty = Minus ty | Plus ty deriving (Eq, Show, Functor, Foldable, Traversable)
newtype Free ty = Free {runFree :: Seq (G ty)}
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Eq, Show, IsList)
instance (Eq ty) => Semigroup (Free ty) where
  Free Empty <> g = g
  g <> Free Empty = g
  Free (as :|> Minus a) <> Free (Plus b :<| bs) | a == b = Free as <> Free bs
  Free (as :|> Plus a) <> Free (Minus b :<| bs) | a == b = Free as <> Free bs
  Free (as :|> a) <> Free (b :<| bs) = Free (as :|> a :|> b) <> Free bs
instance (Eq ty) => Monoid (Free ty) where
  mempty = Free Empty
instance (Eq ty) => Group (Free ty) where
  invert (Free xs) = Free $ flip fix xs \rec -> \case
    Empty -> Empty
    as :|> Minus a -> Plus a :<| rec as
    as :|> Plus a -> Minus a :<| rec as
