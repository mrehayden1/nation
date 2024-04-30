module App.Geometry (
  module App.Geometry.Clip,
  module App.Geometry.Types,

  inAxisAlignedBoundingBox,
  inDisc,
  inPolygon
) where

import Control.Applicative
import Control.Lens
import Linear

import App.Geometry.Clip
import App.Geometry.Types

-- Computes wether a point is inside a polygon using ray casting.
-- If the ray shooting out of the point along the positive-x axis intersects
-- with an odd number of edges then the point is inside the polygon.
inPolygon :: forall a. (Fractional a, Ord a) => Polygon a -> V2 a -> Bool
inPolygon (Polygon faces holes') (V2 xp yp) =
  let edges = concat $ fmap faceEdges faces ++ fmap faceEdges holes'
  in (== 1) . (`mod` 2) . length . filter edgeIntersectsRay $ edges
 where
  faceEdges :: Face a -> [(V2 a, V2 a)]
  faceEdges ps = zip ps . drop 1 . cycle $ ps

  edgeIntersectsRay :: (V2 a, V2 a) -> Bool
  edgeIntersectsRay (V2 x0 y0, V2 x1 y1) =
    (yp < y0) /= (yp < y1)
      && xp < (x0 + (x1 - x0) * (yp - y0) / (y1 - y0))

inDisc :: (Floating a, Ord a) => V2 a -> a -> V2 a -> Bool
inDisc c r p = r >= distance p c

inAxisAlignedBoundingBox :: forall a. Ord a => V2 a -> V2 a -> V2 a -> Bool
inAxisAlignedBoundingBox (V2 x0 y0) (V2 x1 y1) =
  liftA2 (&&) (inBox' x0 x1 _x) (inBox' y0 y1 _y)
 where
  inBox' :: a -> a -> Lens' (V2 a) a -> V2 a -> Bool
  inBox' m n f = liftA2 (/=) ((< m) . view f) ((< n) . view f)

