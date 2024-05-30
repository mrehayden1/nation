module App.Collision.AABB (
  AABB(..),
  HasAABB(..),

  fromVertices,
  leftBounds,
  rightBounds,
  centroid,
  intersecting,

  pairsOverlapping
) where

import Control.Applicative
import Data.Foldable1 as F1
import Data.Distributive
import Data.List.NonEmpty as NE
import Linear

-- An AABB should contain two n-dimensional vectors, the minimum and maximum
-- of the limit defined by the bounds in all bases respectively.
-- e.g. AABB (V3 0 0 0) (V3 1 1 1)
data AABB v = AABB v v
 deriving (Show)

instance Functor AABB where
  fmap f (AABB l r) = AABB (f l) (f r)

instance Foldable AABB where
  foldr f z (AABB l r) = f l (f r z)
  foldl f z (AABB l r) = f (f z l) r

instance (Distributive f, Ord a) => Semigroup (AABB (f a)) where
  (<>) (AABB p0 p1) (AABB q0 q1) = fromVertices . NE.fromList $ [p0, p1, q0, q1]

class HasAABB a v where
  aabb :: a -> AABB v

instance HasAABB (AABB v) v where
  aabb = id

fromVertices :: (Functor f, Foldable1 f, Distributive d, Ord a)
  => f (d a)
  -> AABB (d a)
fromVertices ps =
  let ps' = distribute ps
  in AABB (fmap F1.minimum ps') (fmap F1.maximum ps')

leftBounds :: AABB v -> v
leftBounds (AABB l _) = l

rightBounds :: AABB v -> v
rightBounds (AABB _ r) = r

centroid :: Fractional v => AABB v -> v
centroid (AABB l r) = l + (r - l) / 2

intersecting :: forall t a. (Metric t, Traversable t, Num a, Ord a)
  => AABB (t a)
  -> AABB (t a)
  -> Bool
intersecting (AABB l0 r0) (AABB l1 r1) =
  all (liftA2 pairsOverlapping (pairDot l0 r0) (pairDot l1 r1)) basis
 where
  pairDot :: t a -> t a -> t a -> (a, a)
  pairDot l r = liftA2 (,) (dot l) (dot r)

pairsOverlapping :: Ord a => (a, a) -> (a, a) -> Bool
pairsOverlapping (pMin, pMax) (qMin, qMax) =
  not $ pMin <= qMin && pMax <= qMin || qMin <= pMin && qMax <= pMin
