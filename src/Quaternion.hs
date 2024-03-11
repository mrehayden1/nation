module Quaternion (
  fromVectors
) where

import Linear

fromVectors :: forall a. Floating a => V3 a -> V3 a -> Quaternion a
fromVectors a b =
  let v = a `cross` b
      w = sqrt (mag2 a * mag2 b) + dot a b
      m = sqrt $ mag2 v + w ** 2
  in (/ m) <$> Quaternion w v
 where
  mag2 :: V3 a -> a
  mag2 = sum . fmap (**2)
