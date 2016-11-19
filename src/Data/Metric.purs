-- | Metric spaces.
module Data.Metric
( class Metric
, distance
) where

import Data.Int as Int
import Data.Ord (abs)
import Prelude

-- | Instances must satisfy the following laws:
-- |
-- |  - Separation: `distance x y >= 0.0`
-- |  - Identity of indiscernibles: `distance x y == 0.0` iff `x` = `y`
-- |  - Symmetry: `distance x y == distance y x`
-- |  - Subadditivity: `distance x y =< distance x y + distance y z`
class Metric a where
    distance :: a -> a -> Number

instance metricVoid :: Metric Void where
    distance _ = absurd

instance metricUnit :: Metric Unit where
    distance _ _ = 0.0

instance metricInt :: Metric Int where
    distance a b = Int.toNumber $ abs $ a - b

instance metricNumber :: Metric Number where
    distance a b = abs $ a - b
