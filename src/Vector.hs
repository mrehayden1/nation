module Vector (
  toGlVector3,
  toGlVector4,

  eulerDirection,
  magnitude
) where

import qualified Graphics.Rendering.OpenGL as GL

import Linear

toGlVector3 :: V3 a -> GL.Vector3 a
toGlVector3 (V3 x y z) = GL.Vector3 x y z

toGlVector4 :: V4 a -> GL.Vector4 a
toGlVector4 (V4 x y z w) = GL.Vector4 x y z w

eulerDirection :: (Floating a, Epsilon a) => a -> a -> V3 a
eulerDirection pitch yaw =
  let x = cos yaw * cos pitch
      y = sin pitch
      z = negate $ sin yaw * cos pitch
  in normalize $ V3 x y z

magnitude :: (Functor t, Foldable t, Floating a) => t a -> a
magnitude = sqrt . sum . fmap (**2)
