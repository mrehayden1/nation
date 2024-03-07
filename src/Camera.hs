module Camera (
  Camera (..),

  worldUp,

  direction,
  right,

  toViewMatrix,
  toInverseViewMatrix
) where

import Linear

-- Conventions for our Camera angles
--
-- Our camera is defined as a postion, pitch and yaw in a right-handed
-- euclidean 3D space, where yaw is the angle in radians of the direction
-- vector of the camera projected onto the horizontal plane (y=0), measured
-- anticlockwise from the +x axis, and pitch is similarly measured from the
-- horizontal.

-- TODO Investigate storing the camera state as a matrix.
data Camera = Camera {
  camPitch :: !Float,
  camPos :: !(V3 Float),
  camYaw :: !Float
} deriving (Show)

-- World up unit vector
worldUp :: Floating a => V3 a
worldUp = V3 0 1 0

-- Directional unit vector of the camera given pitch and yaw
direction :: Camera -> V3 Float
direction Camera{..} =
  let x = cos camYaw * cos camPitch
      y = sin camPitch
      z = negate $ sin camYaw * cos camPitch
  in normalize $ V3 x y z

right :: Camera -> V3 Float
right Camera{..} = V3 (sin camYaw) 0 (cos camYaw)

toViewMatrix :: Camera -> M44 Float
toViewMatrix cam@Camera{..} =
  let dir    = direction cam
      -- the 'centre' to which the camera is looking
      centre = camPos + dir
      -- no camera roll so the camera is always on the x-z plane
      up     = right cam `cross` dir -- camera's up
  in lookAt camPos centre up

toInverseViewMatrix :: Camera -> M44 Float
toInverseViewMatrix = inv44 . toViewMatrix
