module Camera (
  Camera (..),

  worldUp,

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

-- TODO Investigate storing the camera state as a matrix.
data Camera a = Camera {
  camPitch :: !a,
  camPos :: !(L.V3 a),
  camYaw :: !a
} deriving (Show)

-- World up unit vector
worldUp :: Floating a => L.V3 a
worldUp = L.V3 0 1 0

-- Directional unit vector of the camera given pitch and yaw
direction :: (Floating a, L.Epsilon a) => Camera a -> L.V3 a
direction Camera{..} =
  let x = cos camYaw * cos camPitch
      y = sin camPitch
      z = negate $ sin camYaw * cos camPitch
  in L.normalize $ L.V3 x y z

toViewMatrix :: (Floating a, L.Epsilon a) => Camera a -> L.M44 a
toViewMatrix cam@Camera{..} =
  let dir    = direction cam
      -- the 'centre' to which the camera is looking
      centre = camPos + dir
      right = L.V3 (sin camYaw) 0 (cos camYaw)
      up     = right `L.cross` dir -- camera's up
  in L.lookAt camPos centre up
