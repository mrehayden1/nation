module App.Entity.Collision3D (
  Collision3D(..),

  transformCollision3d,

  collided3d
) where

import Control.Applicative
import Control.Lens
import Data.Function
import qualified Data.List as L
import Linear

import App.Entity.Collision

import Text.Printf

data Collision3D a =
    CollisionSphere (V3 a) a
    -- Ordered points and face normals for calculating axes of separation
  | CollisionPolyhedron [V3 a] [V3 a]
 deriving (Show)

transformCollision3d :: Floating a => M44 a -> Collision3D a -> Collision3D a
transformCollision3d m (CollisionSphere c r) =
  let c4 = point c
      c' = m !* c4
      p  = m !* (c4 + V4 r 0 0 0)
  in CollisionSphere (c' ^. _xyz) . norm . (^. _xyz) $ (p - c')
transformCollision3d m (CollisionPolyhedron ps ns) =
  let ps' = fmap ((^. _xyz) . (m !*) . point) ps
      normalMatrix = (^. _m33) . transpose . inv44 $ m
      ns' = fmap (normalMatrix !*) ns
  in CollisionPolyhedron ps' ns'

collided3d :: (Epsilon a, Floating a, Ord a, PrintfArg a)
  => Collision3D a -> Collision3D a -> Bool
collided3d   (CollisionSphere c0 r0)         (CollisionSphere c1 r1)   =
  let a = normalize (c1 - c0)
      (min0, max0) = liftA2 (,) (subtract r0) (+ r0) $ c0 `dot` a
      (min1, max1) = liftA2 (,) (subtract r1) (+ r1) $ c1 `dot` a
  in not $ min0 <= min1 && max0 <= min1 || min1 <= min0 && max1 <= min0
collided3d s@(CollisionSphere _ _)         p@(CollisionPolyhedron _ _) =
  collided3d s p
collided3d   (CollisionPolyhedron ps ns)     (CollisionSphere c r)     =
  let -- Axis between the sphere centre and the closest point on the polyhedron
      axis = (`subtract` c)
               . L.minimumBy (compare `on` (quadrance . (`subtract` c)))
               $ ps
  in all (polygonCircleOverlapping ps c r) $ axis : ns
collided3d   (CollisionPolyhedron ps0 ns0)   (CollisionPolyhedron ps1 ns1) =
  all (polygonsOverlapping ps0 ps1) $ ns0 ++ ns1
