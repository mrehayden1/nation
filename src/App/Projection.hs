module App.Projection (
  Frustum,

  frustumPoints,
  frustumFaceNormals,

  cameraFrustum,

  perspectiveProjection,
  inversePerspectiveProjection,

  directionalLightProjection,
  directionalLightViewMatrix
) where

import Control.Lens
import Data.List.NonEmpty as NE
import Linear

import App.Camera as Cam
import App.Collision.AABB
import App.Vector

data Frustum a = Frustum {
  frustumRightTopBack     :: V3 a,
  frustumRightTopFront    :: V3 a,
  frustumLeftTopBack      :: V3 a,
  frustumLeftTopFront     :: V3 a,
  frustumRightBottomBack  :: V3 a,
  frustumRightBottomFront :: V3 a,
  frustumLeftBottomBack   :: V3 a,
  frustumLeftBottomFront  :: V3 a
} deriving (Show)

instance Ord a => HasAABB (Frustum a) (V3 a) where
  aabb (Frustum a b c d e f g h) =
    fromVertices . NE.fromList $ [a, b, c, d, e, f, g, h]

transformFrustum :: (Fractional a) => M44 a -> Frustum a -> Frustum a
transformFrustum m Frustum{..} = Frustum {
    frustumRightTopBack = f frustumRightTopBack,
    frustumRightTopFront = f frustumRightTopFront,
    frustumLeftTopBack = f frustumLeftTopBack,
    frustumLeftTopFront = f frustumLeftTopFront,
    frustumRightBottomBack = f frustumRightBottomBack,
    frustumRightBottomFront = f frustumRightBottomFront,
    frustumLeftBottomBack = f frustumLeftBottomBack,
    frustumLeftBottomFront = f frustumLeftBottomFront
  }
 where
  f = normalizePoint . (m !*) . point

frustumPoints :: Frustum a -> [V3 a]
frustumPoints Frustum{..} = [
    frustumRightTopBack,    frustumRightTopFront,
    frustumLeftTopBack,     frustumLeftTopFront,
    frustumRightBottomBack, frustumRightBottomFront,
    frustumLeftBottomBack,  frustumLeftBottomFront
  ]

frustumFaceNormals :: (Epsilon a, Floating a) => Frustum a -> [V3 a]
frustumFaceNormals Frustum{..} = fmap normalize [
    -- right
    (frustumRightBottomFront - frustumRightBottomBack)
      `cross` (frustumRightTopBack - frustumRightBottomBack),
    -- left
    (frustumLeftBottomBack - frustumLeftBottomFront)
      `cross` (frustumLeftTopFront - frustumLeftBottomFront),
    -- top
    (frustumRightTopBack - frustumRightTopFront)
      `cross` (frustumLeftTopFront - frustumRightTopFront),
    -- bottom
    (frustumRightBottomBack - frustumRightBottomFront)
      `cross` (frustumLeftBottomFront - frustumRightBottomFront),
    -- back
    (frustumRightTopBack - frustumRightBottomBack)
      `cross` (frustumLeftBottomBack - frustumRightBottomBack),
    -- front
    (frustumLeftBottomFront - frustumRightBottomFront)
      `cross` (frustumRightTopFront - frustumRightBottomFront)
  ]

cameraFrustum :: (Epsilon a, Floating a) => a -> Camera a -> Frustum a
cameraFrustum aspectRatio camera =
  let viewM = Cam.toViewMatrix camera
      projectionM = perspectiveProjection aspectRatio
      inverseViewProjection = inv44 $ projectionM !*! viewM
  in transformFrustum inverseViewProjection ndcCube

ndcCube :: Floating a => Frustum a
ndcCube = Frustum {
    frustumRightTopFront    = V3   1    1    1 ,
    frustumRightTopBack     = V3   1    1  (-1),
    frustumLeftTopFront     = V3 (-1)   1    1 ,
    frustumLeftTopBack      = V3 (-1)   1  (-1),
    frustumRightBottomFront = V3   1  (-1)   1 ,
    frustumRightBottomBack  = V3   1  (-1) (-1),
    frustumLeftBottomFront  = V3 (-1) (-1)   1 ,
    frustumLeftBottomBack   = V3 (-1) (-1) (-1)
  }

-- The application global field-of-view (in degrees) and near and far clipping
-- planes.
far, fov, near :: Floating a => a
far  = 25
fov  = 35
near = 0.1

directionalLightProjection :: (Epsilon a, Floating a, Ord a)
  => Camera a
  -> a
  -> a
  -> a
  -> M44 a
directionalLightProjection camera pitch yaw aspectRatio =
  let lightMatrix = directionalLightViewMatrix camera pitch yaw aspectRatio
      lightFrustum = fmap (normalizePoint . (lightMatrix !*) . point)
        . frustumPoints . cameraFrustum aspectRatio $ camera
      -- Get the bounds of the cuboid in light space for our orthographic
      -- projection
      xMin = minimum . fmap (^. _x) $ lightFrustum
      xMax = maximum . fmap (^. _x) $ lightFrustum
      yMin = minimum . fmap (^. _y) $ lightFrustum
      yMax = maximum . fmap (^. _y) $ lightFrustum
      zMax = maximum . fmap (^. _z) $ lightFrustum
      zMin = minimum . fmap (^. _z) $ lightFrustum
  in ortho xMin xMax yMin yMax zMax zMin

perspectiveProjection :: Floating a => a -> M44 a
perspectiveProjection aspectRatio =
  perspective (fov * pi / 180) aspectRatio near far

inversePerspectiveProjection :: Floating a => a -> M44 a
inversePerspectiveProjection aspectRatio =
  inversePerspective (fov * pi / 180) aspectRatio near far

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
      centre = (/ 8) . sum . frustumPoints . cameraFrustum aspectRatio
                 $ camera
      -- no camera roll
      camRight  = V3 (sin yaw) 0 (cos yaw)
      up     = camRight `cross` dir -- camera's up
  in lookAt (centre - dir) centre up
