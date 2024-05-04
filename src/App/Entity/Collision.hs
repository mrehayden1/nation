module App.Entity.Collision (
  Collision(..),

  transformCollision,

  collided,

  polygonCircleOverlapping,
  polygonsOverlapping
) where

import Control.Applicative
import Control.Lens
import Data.Function
import qualified Data.List as L
import Linear

import App.Vector

-- Collision detection is implemented in 2-dimensions using the separating axis
-- theorem where each entity's collision geometry is defined as a shape in the
-- x/z plane relative to the model geometry origin measured in metres.
data Collision a =
    -- Centre and radius of a circle
    CollisionCircle (V2 a) a
    -- Ordered points of a convex polygon
  | CollisionPolygon [V2 a]
 deriving (Show)

transformCollision :: Floating a => M33 a -> Collision a -> Collision a
transformCollision m (CollisionCircle c r) =
  let c3 = point2d c -- Project into 3d space to apply the transformation
      c' = m !* c3
      p  = m !* (c3 + V3 r 0 0)
  in CollisionCircle (c' ^. _xy) . norm . (^. _xy) $ (p - c')
transformCollision m (CollisionPolygon ps) =
  CollisionPolygon . fmap ((^. _xy) . (m !*) . point2d) $ ps

-- Project each collision shape onto every possible axis of separation
-- according to the separating axis theorem. For circles, take the axis
-- between the centre point of each shape for its separating axis.
collided :: (Epsilon a, Floating a, Ord a)
  => Collision a
  -> Collision a
  -> Bool
collided   (CollisionCircle c0 r0)   (CollisionCircle c1 r1) =
  let a  = normalize (c1 - c0)
      (min0, max0) = liftA2 (,) (subtract r0) (+ r0) $ c0 `dot` a
      (min1, max1) = liftA2 (,) (subtract r1) (+ r1) $ c1 `dot` a
  in not $ min0 <= min1 && max0 <= min1 || min1 <= min0 && max1 <= min0
collided c@(CollisionCircle _ _)   p@(CollisionPolygon _)    =
  collided p c
collided   (CollisionPolygon ps)     (CollisionCircle c r)   =
  let -- Axis of collision for nearest point on polygon to circle center
      axis = (`subtract` c)
               . L.minimumBy (compare `on` (quadrance . (`subtract` c)))
               $ ps
      -- Axes of collision for each polygon face
      axes = fmap (\(V2 x y) -> V2 (-y) x) . calcPolygonAxes $ ps
  in all (polygonCircleOverlapping ps c r) $ axis : axes
collided   (CollisionPolygon ps0)    (CollisionPolygon ps1)  =
  all (polygonsOverlapping ps0 ps1)
      (calcPolygonAxes ps0 ++ calcPolygonAxes ps1)

calcPolygonAxes :: (Epsilon a, Floating a) => [V2 a] -> [V2 a]
calcPolygonAxes ps = fmap (getNormal . normalize . uncurry (flip (-)))
  . zip ps . drop 1
  . cycle $ ps
 where
  getNormal (V2 x y) = V2 (-y) x

-- Project a polygon and circle onto an axis of collision and check if they
-- overlap
polygonCircleOverlapping :: (Metric f, Num a, Ord a)
  => [f a] -> f a -> a -> f a -> Bool
polygonCircleOverlapping ps c r axis =
  let (pMin, pMax) = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ps
      (cMin, cMax) = liftA2 (,) (subtract r) (+ r) $ c `dot` axis
  in not $ pMin <= cMin && pMax <= cMin || cMin <= pMin && cMax <= pMin

-- Project two polygons onto an axis of collision and check if they overlap
polygonsOverlapping :: (Metric f, Num a, Ord a)
  => [f a] -> [f a] -> f a -> Bool
polygonsOverlapping ps ss axis =
  let (pMin, pMax) = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ps
      (sMin, sMax) = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ss
  in not $ pMin <= sMin && pMax <= sMin || sMin <= pMin && sMax <= pMin
