module App.Matrix (
  scale,
  scale2D,
  translate,
  translate2D,
  rotateX,
  rotateY,
  rotateZ,
  rotate2D,

  toGlMatrix
) where

import Data.Foldable
import qualified Graphics.Rendering.OpenGL as GL
import Linear

scale :: Num a => V3 a -> M44 a
scale (V3 x y z) =
  V4 (V4 x 0 0 0)
     (V4 0 y 0 0)
     (V4 0 0 z 0)
     (V4 0 0 0 1)

scale2D :: Num a => V2 a -> M33 a
scale2D (V2 x y) =
  V3 (V3 x 0 0)
     (V3 0 y 0)
     (V3 0 0 1)

translate :: Num a => V3 a -> M44 a
translate (V3 x y z) =
  V4 (V4 1 0 0 x)
     (V4 0 1 0 y)
     (V4 0 0 1 z)
     (V4 0 0 0 1)

translate2D :: Num a => V2 a -> M33 a
translate2D (V2 x y) =
  V3 (V3 1 0 x)
     (V3 0 1 y)
     (V3 0 0 1)

rotateX :: Floating a => a -> M44 a
rotateX t =
  V4 (V4 1       0        0 0)
     (V4 0 (cos t) (-sin t) 0)
     (V4 0 (sin t) ( cos t) 0)
     (V4 0       0        0 1)

rotateY :: Floating a => a -> M44 a
rotateY t =
  V4 (V4 ( cos t) 0 (sin t) 0)
     (V4        0 1       0 0)
     (V4 (-sin t) 0 (cos t) 0)
     (V4        0 0       0 1)

rotateZ :: Floating a => a -> M44 a
rotateZ t =
  V4 (V4 (cos t) (-sin t) 0 0)
     (V4 (sin t) ( cos t) 0 0)
     (V4       0        0 1 0)
     (V4       0        0 0 1)

rotate2D :: Floating a => a -> M33 a
rotate2D t =
  V3 (V3 (cos t) (-sin t) 0)
     (V3 (sin t) ( cos t) 0)
     (V3      0        0  1)

-- unpack in row major order
unpack :: (Foldable t1, Foldable t2) => t1 (t2 a) -> [a]
unpack = concatMap toList . toList

-- unpacks nested vectors in row major order into a new GLmatrix
toGlMatrix :: (GL.MatrixComponent a, Foldable t1, Foldable t2)
  => t1 (t2 a)
  -> IO (GL.GLmatrix a)
toGlMatrix = GL.newMatrix GL.RowMajor . unpack
