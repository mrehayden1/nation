module App.Matrix (
  perspectiveProjection,
  inversePerspectiveProjection,

  directionalLightProjection,
  directionalLightViewMatrix,

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

import Control.Lens
import Data.Foldable
import Data.Tuple.Extra
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import App.Camera as Cam
import App.Vector

directionalLightProjection :: (Epsilon a, Floating a, Ord a)
  => Camera a
  -> a
  -> a
  -> a
  -> M44 a
directionalLightProjection camera pitch yaw aspectRatio =
  let lightMatrix = directionalLightViewMatrix camera pitch yaw aspectRatio
      lightFrustum = fmap (normalizePoint . (lightMatrix !*) . point)
        . cameraFrustum aspectRatio $ camera
      planeLeft = minimum . fmap (^. _x) $ lightFrustum
      planeRight = maximum . fmap (^. _x) $ lightFrustum
      planeBottom = minimum . fmap (^. _y) $ lightFrustum
      planeTop = maximum . fmap (^. _y) $ lightFrustum
      planeNear = maximum . fmap (^. _z) $ lightFrustum
      planeFar = minimum . fmap (^. _z) $ lightFrustum
  in ortho planeLeft planeRight planeBottom planeTop planeNear planeFar

-- The application global field of view angle (in degrees) and near and far
-- clipping planes.
far, fov, near :: Floating a => a
far  = 25
fov  = 35
near = 0.1

-- Light direction points towards the (infinitely far away) light source
directionalLightViewMatrix :: (Epsilon a, Floating a)
  => Camera a
  -> a
  -> a
  -> a
  -> M44 a
directionalLightViewMatrix camera pitch yaw aspectRatio =
  -- TODO Make a function of the camera view
  let dir    = eulerDirection pitch yaw
      -- the 'centre' to which the camera is looking
      centre = (/ 8) . sum . cameraFrustum aspectRatio $ camera
      -- no camera roll
      camRight  = V3 (sin yaw) 0 (cos yaw)
      up     = camRight `cross` dir -- camera's up
  in lookAt (centre - dir) centre up

cameraFrustum :: (Epsilon a, Floating a) => a -> Camera a -> [V3 a]
cameraFrustum aspectRatio camera =
  let viewM = Cam.toViewMatrix camera
      projectionM = perspectiveProjection aspectRatio
      inverseViewProjection =
        inv44 $ projectionM !*! viewM
  in fmap (normalizePoint . (inverseViewProjection !*)) ndcCube
 where
  ndcCube :: Floating a => [V4 a]
  ndcCube = fmap (point . uncurry3 V3) [
    ( 1,  1, -1), ( 1,  1,  1), (-1,  1,  1), (-1,  1, -1),
    ( 1, -1, -1), ( 1, -1,  1), (-1, -1,  1), (-1, -1, -1)]

perspectiveProjection :: Floating a => a -> M44 a
perspectiveProjection aspectRatio =
  perspective (fov * pi / 180) aspectRatio near far

inversePerspectiveProjection :: Floating a => a -> M44 a
inversePerspectiveProjection aspectRatio =
  inversePerspective (fov * pi / 180) aspectRatio near far

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
