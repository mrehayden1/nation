module Vector (
  cameraDirection,

  toGlVector3,
  toGlVector4
) where

import qualified Graphics.Rendering.OpenGL as GL

import Linear

toGlVector3 :: V3 a -> GL.Vector3 a
toGlVector3 (V3 x y z) = GL.Vector3 x y z
 
toGlVector4 :: V4 a -> GL.Vector4 a
toGlVector4 (V4 x y z w) = GL.Vector4 x y z w

cameraDirection :: (Floating a, Epsilon a) => a -> a -> V3 a
cameraDirection pitch yaw =
  let x = cos yaw * cos pitch
      y = sin pitch
      z = negate $ sin yaw * cos pitch
  in normalize $ V3 x y z
