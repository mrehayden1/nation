module Matrix (
  rotateX,
  rotateY,
  rotateZ,

  unpack
) where

import Data.Foldable

import qualified Linear as L
import Linear (M44, V4(..))

rotateX :: Floating a => a -> M44 a
rotateX t =
  V4
    (V4 1 0 0 0)
    (V4 0 (cos t) (negate $ sin t) 0)
    (V4 0 (sin t) (cos t) 0)
    (V4 0 0 0 1)

rotateY :: Floating a => a -> L.M44 a
rotateY t =
  V4
    (V4 (cos t) 0 (sin t) 0)
    (V4 0 1 0 0)
    (V4 (negate $ sin t) 0 (cos t) 0)
    (V4 0 0 0 1)


rotateZ :: Floating a => a -> L.M44 a
rotateZ t =
  V4
    (V4 (cos t) (negate $ sin t) 0 0)
    (V4 (sin t) (cos t) 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

unpack :: L.M44 a -> [a]
unpack = concatMap toList . toList 
