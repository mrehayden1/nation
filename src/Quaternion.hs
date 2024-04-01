module Quaternion (
  fromVectors,

  identityQuaternion
) where

import Linear

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

identityQuaternion :: Quaternion Float
identityQuaternion = Quaternion 1 0
