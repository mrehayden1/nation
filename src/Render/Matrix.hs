module Render.Matrix (
  perspectiveProjection,

  directionalLightProjection,
  directionalLightViewMatrix,

  scale,
  translate,
  rotateX,
  rotateY,
  rotateZ,

  toGlMatrix
) where

import Data.Foldable
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import Linear (Epsilon(..), V3(..), V4(..), M44)

import Vector

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
  => a
  -> a
  -> M44 a
directionalLightViewMatrix pitch yaw =
  -- TODO Make a function of the camera view
  let dir    = cameraDirection pitch yaw
      -- the 'centre' to which the camera is looking
      centre = V3 0 0 0
      -- no camera roll so the camera is always on the x-z plane
      right  = L.V3 (sin yaw) 0 (cos yaw)
      up     = right `L.cross` dir -- camera's up
  in L.lookAt (negate dir) centre up

perspectiveProjection :: Floating a => a -> a -> a -> M44 a
perspectiveProjection near far aspectRatio =
  L.perspective (fov * pi / 180) aspectRatio near far
 where fov = 45

scale :: Num a => V3 a -> M44 a
scale (V3 x y z) = V4 (V4 x 0 0 0)
                      (V4 0 y 0 0)
                      (V4 0 0 z 0)
                      (V4 0 0 0 1)

translate :: Num a => V3 a -> M44 a
translate (V3 x y z) =
  V4 (V4 1 0 0 x)
     (V4 0 1 0 y)
     (V4 0 0 1 z)
     (V4 0 0 0 1)

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

-- unpack in row major order
unpack :: (Foldable t1, Foldable t2) => t1 (t2 a) -> [a]
unpack = concatMap toList . toList

-- unpacks nested vectors in row major order into a new GLmatrix
toGlMatrix :: (GL.MatrixComponent a, Foldable t1, Foldable t2)
  => t1 (t2 a)
  -> IO (GL.GLmatrix a)
toGlMatrix = GL.newMatrix GL.RowMajor . unpack
