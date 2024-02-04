module Camera (
  Camera (..),

  up,
  direction,
  toViewMatrix
) where

import qualified Linear as L

-- Conventions for our Camera angles
--
-- Our camera is defined as a postion, pitch and yaw in a right-handed
-- euclidean 3D space, where yaw is the angle in radians of the direction
-- vector of the camera projected onto the horizontal plane (y=0), measured
-- anticlockwise from the +x axis, and pitch is similarly measured from the
-- horizontal.

data Camera a = Camera {
  pitch :: !a,
  position :: !(L.V3 a),
  yaw :: !a
} deriving (Show)

up :: Num a => L.V3 a
up = L.V3 0 1 0
   
direction :: (Floating a, L.Epsilon a) => Camera a -> L.V3 a
direction Camera{..} =
  let x = cos yaw * cos pitch
      y = sin pitch
      z = sin yaw * cos pitch
  in L.normalize $ L.V3 x y z

toViewMatrix :: (Floating a, L.Epsilon a) => Camera a -> L.M44 a
toViewMatrix camera =
      -- the 'centre' to which the camera is looking
  let centre    = position camera + direction camera
  in L.lookAt (position camera) centre up
