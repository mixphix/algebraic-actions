module Util where

import GHC.TypeLits (KnownNat, type (+))
import GHC.TypeLits.Witnesses

with2n :: forall n r. ((KnownNat (n + n)) => r) -> ((KnownNat n) => r)
with2n r = case SNat @n %+ SNat @n of SNat -> r
