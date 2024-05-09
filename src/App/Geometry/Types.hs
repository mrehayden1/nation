module App.Geometry.Types (
  Face,
  Polygon(..),

  Tristrip(..)
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

newtype Tristrip a = Tristrip {
  unTristrip :: [[V2 a]]
}
