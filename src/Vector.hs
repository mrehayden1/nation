module Vector (
  direction,

  toGlVector3
) where

import qualified Graphics.Rendering.OpenGL as GL

import Linear (V3 (..))
import qualified Linear as L

toGlVector3 :: V3 a -> GL.Vector3 a
toGlVector3 (V3 x y z) = GL.Vector3 x y z

direction :: (Floating a, L.Epsilon a) => a -> a -> L.V3 a
direction pitch yaw =
  let x = cos yaw * cos pitch
      y = sin pitch
      z = negate $ sin yaw * cos pitch
  in L.normalize $ L.V3 x y z
