module App.Collision (
  Collision2(..),

  transformCollision2,
  collided2,

  Collision3(..),

  transformCollision3,
  collided3
) where

import Control.Applicative
import Control.Lens
import Data.Function
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Linear

import App.Vector
import App.Collision.AABB

-- Collision detection is implemented in 2-dimensions using the separating axis
-- theorem where collision geometry is defined as a convex shape in the plane
data Collision2 a =
    -- Centre and radius of a circle
    CollisionCircle (V2 a) a
    -- Ordered points of a convex polygon
  | CollisionPolygon [V2 a]
 deriving (Show)

transformCollision2 :: Floating a => M33 a -> Collision2 a -> Collision2 a
transformCollision2 m (CollisionCircle c r) =
  let c3 = point2d c -- Project into 3d space to apply the transformation
      c' = m !* c3
      p  = m !* (c3 + V3 r 0 0)
  in CollisionCircle (c' ^. _xy) . norm . (^. _xy) $ (p - c')
transformCollision2 m (CollisionPolygon ps) =
  CollisionPolygon . fmap ((^. _xy) . (m !*) . point2d) $ ps

-- Project each collision shape onto every possible axis of separation
-- according to the separating axis theorem. For circles, take the axis
-- between the centre point of each shape for its separating axis.
collided2 :: (Epsilon a, Floating a, Ord a)
  => Collision2 a
  -> Collision2 a
  -> Bool
collided2   (CollisionCircle c0 r0)   (CollisionCircle c1 r1) =
  let a  = normalize (c1 - c0)
      circle0MinMax = liftA2 (,) (subtract r0) (+ r0) $ c0 `dot` a
      circle1MinMax = liftA2 (,) (subtract r1) (+ r1) $ c1 `dot` a
  in pairsOverlapping circle0MinMax circle1MinMax
collided2 c@(CollisionCircle _ _)   p@(CollisionPolygon _)    =
  collided2 p c
collided2   (CollisionPolygon ps)     (CollisionCircle c r)   =
  let -- Axis of collision for nearest point on polygon to circle center
      axis = (`subtract` c)
               . L.minimumBy (compare `on` (quadrance . (`subtract` c)))
               $ ps
      -- Axes of collision for each polygon face
      axes = fmap (\(V2 x y) -> V2 (-y) x) . calcPolygonAxes $ ps
  in all (polygonCircleOverlappingOnAxis ps c r) $ axis : axes
collided2   (CollisionPolygon ps0)    (CollisionPolygon ps1)  =
  all (polygonsOverlappingOnAxis ps0 ps1)
      (calcPolygonAxes ps0 ++ calcPolygonAxes ps1)

calcPolygonAxes :: (Epsilon a, Floating a) => [V2 a] -> [V2 a]
calcPolygonAxes ps = fmap (getNormal . normalize . uncurry (flip (-)))
  . zip ps . drop 1 . cycle $ ps
 where
  getNormal (V2 x y) = V2 (-y) x

data Collision3 a =
    CollisionSphere (V3 a) a
    -- Ordered points and face normals for calculating axes of separation
  | CollisionPolyhedron [V3 a] [V3 a]
 deriving (Show)

instance (Num a, Ord a) => HasAABB (Collision3 a) (V3 a) where
  aabb (CollisionSphere c r)      = AABB (c - V3 r r r) (c + V3 r r r)
  aabb (CollisionPolyhedron ps _) = fromVertices . NE.fromList $ ps

transformCollision3 :: Floating a => M44 a -> Collision3 a -> Collision3 a
transformCollision3 m (CollisionSphere c r) =
  let c4 = point c
      c' = m !* c4
      p  = m !* (c4 + V4 r 0 0 0)
  in CollisionSphere (c' ^. _xyz) . norm . (^. _xyz) $ (p - c')
transformCollision3 m (CollisionPolyhedron ps ns) =
  let ps' = fmap ((^. _xyz) . (m !*) . point) ps
      normalMatrix = (^. _m33) . transpose . inv44 $ m
      ns' = fmap (normalMatrix !*) ns
  in CollisionPolyhedron ps' ns'

collided3 :: (Epsilon a, Floating a, Ord a)
  => Collision3 a -> Collision3 a -> Bool
collided3   (CollisionSphere c0 r0)         (CollisionSphere c1 r1)   =
  let a  = normalize (c1 - c0)
      circle0MinMax = liftA2 (,) (subtract r0) (+ r0) $ c0 `dot` a
      circle1MinMax = liftA2 (,) (subtract r1) (+ r1) $ c1 `dot` a
  in pairsOverlapping circle0MinMax circle1MinMax
collided3 s@(CollisionSphere _ _)         p@(CollisionPolyhedron _ _) =
  collided3 s p
collided3   (CollisionPolyhedron ps ns)     (CollisionSphere c r)     =
  let -- Axis between the sphere centre and the closest point on the polyhedron
      axis = (`subtract` c)
               . L.minimumBy (compare `on` (quadrance . (`subtract` c)))
               $ ps
  in all (polygonCircleOverlappingOnAxis ps c r) $ axis : ns
collided3   (CollisionPolyhedron ps0 ns0)   (CollisionPolyhedron ps1 ns1) =
  all (polygonsOverlappingOnAxis ps0 ps1) $ ns0 ++ ns1

-- Project a polygon and circle onto an axis of collision and check if they
-- overlap
polygonCircleOverlappingOnAxis :: (Metric f, Num a, Ord a)
  => [f a] -> f a -> a -> f a -> Bool
polygonCircleOverlappingOnAxis ps c r axis =
  let pMinMax = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ps
      cMinMax = liftA2 (,) (subtract r) (+ r) $ c `dot` axis
  in pairsOverlapping pMinMax cMinMax

-- Project two polygons onto an axis of collision and check if they overlap
polygonsOverlappingOnAxis :: (Metric f, Num a, Ord a)
  => [f a] -> [f a] -> f a -> Bool
polygonsOverlappingOnAxis ps ss axis =
  let pMinMax = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ps
      sMinMax = liftA2 (,) minimum maximum . fmap (`dot` axis) $ ss
  in pairsOverlapping pMinMax sMinMax
