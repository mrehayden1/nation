module App.Geometry.Types (
  Polygon(..)
) where

import Linear

type Face a = [V2 a]

data Polygon a = Polygon {
  polygonFaces :: [Face a],
  polygonHoles :: [Face a]
} deriving (Show)

instance Functor Polygon where
  fmap f (Polygon faces holes) =
    Polygon (fmap (fmap (fmap f)) faces) (fmap (fmap (fmap f)) holes)
