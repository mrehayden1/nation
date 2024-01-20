module Vector (
  toGlVector3
) where

import qualified Graphics.Rendering.OpenGL as GL

import Linear (V3 (..))

toGlVector3 :: V3 a -> GL.Vector3 a
toGlVector3 (V3 x y z) = GL.Vector3 x y z 
