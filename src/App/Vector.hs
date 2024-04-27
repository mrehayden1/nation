module App.Vector (
  toGlVector3,
  toGlVector4,

  eulerDirection,
  magnitude,
  magnitude2,

  point2D
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
magnitude = sqrt . magnitude2

magnitude2 :: (Functor t, Foldable t, Floating a) => t a -> a
magnitude2 = sum . fmap (**2)

point2D :: Num a => V2 a -> V3 a
point2D (V2 x y) = V3 x y 1
