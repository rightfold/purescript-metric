-- | Metric spaces.
module Data.Metric
( class Metric
, distance
) where

import Data.Ord (abs)
import Prelude

-- | Instances must satisfy the following laws:
-- |
-- |  - Separation: `distance x y >= zero`
-- |  - Identity of indiscernibles: `distance x y == zero` iff `x` = `y`
-- |  - Symmetry: `distance x y == distance y x`
-- |  - Subadditivity: `distance x y =< distance x y + distance y z`
class (Semiring d, Ord d) <= Metric a d | a -> d where
    distance :: a -> a -> d

instance metricVoid :: (Semiring d, Ord d) => Metric Void d where
    distance _ = absurd

instance metricUnit :: (Semiring d, Ord d) => Metric Unit d where
    distance _ _ = zero

instance metricInt :: Metric Int Int where
    distance a b = abs $ a - b

instance metricNumber :: Metric Number Number where
    distance a b = abs $ a - b
