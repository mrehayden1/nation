module App.Entity.Collision (
  Collision(..),

  transformCollision,

  collided
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
data Collision =
    -- Centre and radius
    CollisionCircle (V2 Float) Float
    -- Convex quadrilateral
  | CollisionPolygon [V2 Float]
 deriving (Show)

transformCollision :: M33 Float -> Collision -> Collision
transformCollision m (CollisionCircle c r) =
  let c3 = point2D c
      c' = m !* c3
      p  = m !* (c3 + V3 r 0 0)
  in CollisionCircle (c' ^. _xy) . magnitude . (^. _xy) $ (p - c')
transformCollision m (CollisionPolygon ps) =
  CollisionPolygon . fmap ((^. _xy) . (m !*) . point2D) $ ps

-- Project each collision shape onto every possible axis of separation
-- according to the separating axis theorem. For circles, take the axis
-- between the centre point of each shape for its separating axis.
collided :: Collision
  -> Collision
  -> Bool
collided    (CollisionCircle c0 r0)  (CollisionCircle c1 r1) =
  let a  = normalize (c1 - c0)
      (min0, max0) = liftA2 (,) (subtract r0) (+ r0) $ c0 `dot` a
      (min1, max1) = liftA2 (,) (subtract r1) (+ r1) $ c1 `dot` a
  in max0 >= min1 && min0 <= max1
collided s0@(CollisionCircle _ _) s1@(CollisionPolygon _)    =
  collided s1 s0
collided    (CollisionPolygon ps)    (CollisionCircle c r)   =
  let vs  = fmap (normalize . uncurry (flip (-))) . zip ps . drop 1
              . cycle $ ps
      -- Axis between the circle centre and the closest point on the polygon
      axis = (`subtract` c)
               . L.minimumBy (compare `on` (magnitude2 . (`subtract` c)))
               $ ps
      -- Axes of collision for each polygon face
      axes = fmap (\(V2 x y) -> V2 (-y) x) vs
  in all (polygonCircleOverlapping ps c r) $ axis : axes
collided    (CollisionPolygon ps0)   (CollisionPolygon ps1)  =
  let vs0 = fmap (normalize . uncurry (flip (-))) . zip ps0 . drop 1
              . cycle $ ps0
      vs1 = fmap (normalize . uncurry (flip (-))) . zip ps1 . drop 1
              . cycle $ ps1
      axes = fmap (\(V2 x y) -> V2 (-y) x) $ vs0 ++ vs1
  in all (polygonsOverlapping ps0 ps1) axes

-- Project a polygon and circle onto an axis of collision and check if they
-- overlap
polygonCircleOverlapping :: [V2 Float] -> V2 Float -> Float -> V2 Float -> Bool
polygonCircleOverlapping ps c r axis =
  let min0 = minimum . fmap (`dot` axis) $ ps
      max0 = maximum . fmap (`dot` axis) $ ps
      (min1, max1) = liftA2 (,) (subtract r) (+ r) $ c `dot` axis
  in max0 >= min1 && min0 <= max1

-- Project two polygons onto an axis of collision and check if they overlap
polygonsOverlapping :: [V2 Float] -> [V2 Float] -> V2 Float -> Bool
polygonsOverlapping ps0 ps1 axis =
  let min0 = minimum . fmap (`dot` axis) $ ps0
      max0 = maximum . fmap (`dot` axis) $ ps0
      min1 = minimum . fmap (`dot` axis) $ ps1
      max1 = maximum . fmap (`dot` axis) $ ps1
  in max0 >= min1 && min0 <= max1
