module App.Collision.BVH (
  BVH,
  create,
  create',

  spatialBisection,

  AABB,
  HasAABB(..),

  query
) where

import Control.Applicative
import Control.Lens
import Data.Distributive
import Data.Foldable as F
import Data.Foldable1 as F1
import Data.Function
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Linear

import App.Collision.AABB

data BVH a v = BVH (AABB v) (BVH a v) (BVH a v)
  | BVHLeaf (AABB v) [a]
  | BVHEmpty
 deriving (Show)

  -- Partition given a list of objects and their AABBs, and the AABB of all
  -- objects.
type AABBPartitionFunction a b f = [(a, AABB (f b))]
       -> AABB (f b)
       -> ([(a, AABB (f b))], [(a, AABB (f b))])

-- Create a new BVH from a partitioning function that divides a list of objects
-- into a left and right hand side.
create :: (HasAABB a (f b), Distributive f, Num (f b), Ord b)
  => AABBPartitionFunction a b f
  -> [a]
  -> BVH a (f b)
create _ [] = BVHEmpty
create p as = create' p . fmap (liftA2 (,) id aabb) $ as

create' :: (Distributive f, Num (f b), Ord b)
  => AABBPartitionFunction a b f
  -> [(a, AABB (f b))]
  -> BVH a (f b)
create' p as =
  let partition = p as bounds
      bounds = foldl1' (<>) . fmap (aabb . snd) . NE.fromList $ as
  in case partition of
       ([] , [] ) -> BVHEmpty
       ([] , rhs) -> BVHLeaf bounds . fmap fst $ rhs
       (lhs, [] ) -> BVHLeaf bounds . fmap fst $ lhs
       (lhs, rhs) -> BVH bounds (create' p lhs) (create' p rhs)

query :: (AABB v -> Bool) -> BVH a v -> [a]
query _ BVHEmpty       = []
query f (BVHLeaf b xs) = if f b then xs else []
query f (BVH b l r)    = if f b then query f l ++ query f r else []

spatialBisection :: (Fractional b, Ord b, R3 f, Fractional (f b))
  => AABBPartitionFunction a b f
spatialBisection as bounds =
  -- Find the longest axis of the containing AABB
  --  someone suggested `holesof traversed` to generate bases lenses for
  --  arbitrary vector spaces
  let bases = [_x, _y, _z]
      baseDims = mapM (liftA2 (.) (,) ((foldl' subtract 0 .) . fmap . view))
                   bases
                   bounds
      base = fst . F.maximumBy (compare `on` snd) $ baseDims
      mid  = centroid . fmap (view base) $ bounds
      f = (> mid) . view base . centroid . snd
  in L.partition f as
