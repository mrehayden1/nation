module Quaternion (
  fromVectors,

  identity
) where

import Linear hiding (identity)

fromVectors :: forall a. (Epsilon a, Floating a)
  => V3 a
  -> V3 a
  -> Quaternion a
fromVectors a b =
  let v = a `cross` b
      w = sqrt (mag2 a * mag2 b) + dot a b
  in normalize . Quaternion w $ v
 where
  mag2 :: V3 a -> a
  mag2 = sum . fmap (**2)

identity :: Quaternion Float
identity = Quaternion 1 0
