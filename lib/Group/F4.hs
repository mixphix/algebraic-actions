module Group.F4 where

import Data.Group

data Four
  = Omit
  | Need
  | Free
  | Many
  deriving (Eq, Ord, Enum, Read, Show)

newtype F4a = F4a Four deriving (Eq, Ord)
instance Semigroup F4a where
  F4a Omit <> F4a x = F4a x
  F4a Need <> F4a x = F4a case x of
    Omit -> Need
    Need -> Omit
    Free -> Many
    Many -> Free
  F4a Many <> F4a x = F4a case x of
    Omit -> Many
    Need -> Free
    Free -> Need
    Many -> Omit
  F4a Free <> F4a x = F4a case x of
    Omit -> Free
    Need -> Many
    Free -> Omit
    Many -> Need
instance Monoid F4a where mempty = F4a Omit
instance Group F4a where
  invert = \case
    F4a Omit -> F4a Need
    F4a Need -> F4a Omit
    F4a Free -> F4a Many
    F4a Many -> F4a Free

newtype F4m = F4m Four deriving (Eq, Ord)
instance Semigroup F4m where
  F4m Omit <> F4m _ = F4m Omit
  F4m Need <> F4m x = F4m x
  F4m Free <> F4m x = F4m case x of
    Omit -> Omit
    Need -> Free
    Many -> Need
    Free -> Many
  F4m Many <> F4m x = F4m case x of
    Omit -> Omit
    Need -> Many
    Many -> Free
    Free -> Need
instance Monoid F4m where mempty = F4m Need

instance Num Four where
  x + y = let F4a z = F4a x <> F4a y in z
  x * y = let F4m z = F4m x <> F4m y in z
  negate x = let F4a nx = invert (F4a x) in nx
  abs = \case
    Omit -> Omit
    Need -> Omit
    Free -> Need
    Many -> Need
  signum = \case
    Omit -> Omit
    Need -> Need
    Free -> Omit
    Many -> Need
  fromInteger n = case n `mod` 4 of
    0 -> Omit
    1 -> Need
    2 -> Free
    _ -> Many
