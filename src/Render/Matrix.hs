module Render.Matrix (
  perspectiveProjection,

  directionalLightProjection,
  directionalLightViewMatrix,

  translate,
  scale,
  rotateX,
  rotateY,
  rotateZ,

  toGlMatrix
) where

import Data.Foldable
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import Linear (Epsilon(..), V3(..), V4(..), M44)

-- Calculate the projection needed to build a depth map for directional light
directionalLightProjection :: (Epsilon a, Floating a) => M44 a
directionalLightProjection = L.ortho (-10) 10 (-10) 10 10 (-10)

{-
type DirectionalLight a = Floating a => V3 a

directionalLightProjection :: (Epsilon a, Floating a) => Camera a -> DirectionalLight a -> M44 a
directionalLightProjection camera lightDirection = 
  let cameraView = Cam.toViewMatrix camera
      cameraProjection = perspectiveProjection
      cameraInverseViewProjection = L.inv44 $ cameraProjection !*! cameraView
      frustum = fmap ((cameraInverseViewProjection !*) . L.point) ndcCube
      lightMatrix = directionalLightViewMatrix (negate lightDirection)
      lightSpaceFrustum = fmap (L.normalizePoint . (lightMatrix !*)) frustum
      top = maximum . fmap (^. L._y) $ lightSpaceFrustum
      bottom = minimum . fmap (^. L._y) $ lightSpaceFrustum
      left = minimum . fmap (^. L._x) $ lightSpaceFrustum
      right = maximum . fmap (^. L._x) $ lightSpaceFrustum
      near = maximum . fmap (^. L._z) $ lightSpaceFrustum
      far = minimum . fmap (^. L._z) $ lightSpaceFrustum
  in L.ortho left right bottom top near far
 where
  ndcCube :: Floating a => [V3 a]
  ndcCube = fmap (\(x, y, z) -> V3 x y z) [
    ( 1,  1, -1), ( 1,  1,  1), (-1,  1,  1), (-1,  1, -1),
    ( 1, -1, -1), ( 1, -1,  1), (-1, -1,  1), (-1, -1, -1)]
-}

-- Light direction points towards the (infinitely far away) light source
directionalLightViewMatrix :: (Epsilon a, Floating a)
  => V3 a
  -> V3 a
  -> M44 a
directionalLightViewMatrix direction =
  L.lookAt (negate direction) (V3 0 0 0)

perspectiveProjection :: Floating a => a -> a -> a -> a -> M44 a
perspectiveProjection near far fov aspectRatio =
  L.perspective (fov * pi / 180) aspectRatio near far

scale :: Num a => a -> M44 a
scale s =
  V4
    (V4 s 0 0 0)
    (V4 0 s 0 0)
    (V4 0 0 s 0)
    (V4 0 0 0 1)

translate :: Num a => a -> a -> a -> M44 a
translate x y z =
  V4
    (V4 1 0 0 x)
    (V4 0 1 0 y)
    (V4 0 0 1 z)
    (V4 0 0 0 1)

rotateX :: Floating a => a -> M44 a
rotateX t =
  V4
    (V4 1 0 0 0)
    (V4 0 (cos t) (negate $ sin t) 0)
    (V4 0 (sin t) (cos t) 0)
    (V4 0 0 0 1)

rotateY :: Floating a => a -> M44 a
rotateY t =
  V4
    (V4 (cos t) 0 (sin t) 0)
    (V4 0 1 0 0)
    (V4 (negate $ sin t) 0 (cos t) 0)
    (V4 0 0 0 1)


rotateZ :: Floating a => a -> M44 a
rotateZ t =
  V4
    (V4 (cos t) (negate $ sin t) 0 0)
    (V4 (sin t) (cos t) 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

-- unpack in row major order
unpack :: (Foldable t1, Foldable t2) => t1 (t2 a) -> [a]
unpack = concatMap toList . toList 

-- unpacks nested vectors in row major order into a new GLmatrix
toGlMatrix :: (GL.MatrixComponent a, Foldable t1, Foldable t2)
  => t1 (t2 a)
  -> IO (GL.GLmatrix a)
toGlMatrix = GL.newMatrix GL.RowMajor . unpack
