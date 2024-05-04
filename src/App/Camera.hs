module App.Camera (
  Camera (..),

  worldUp,

  direction,
  right,

  toViewMatrix,
  toInverseViewMatrix
) where

import Linear

import App.Vector

-- Conventions for our Camera angles
--
-- Our camera is defined as a postion, pitch and yaw in a right-handed
-- euclidean 3D space, where yaw is the angle in radians of the direction
-- vector of the camera projected onto the horizontal plane (y=0), measured
-- anticlockwise from the +x axis, and pitch is similarly measured from the
-- horizontal.

data Camera a = Camera {
  camPitch :: !a,
  camPos :: !(V3 a),
  camYaw :: !a
} deriving (Show)

-- World up unit vector
worldUp :: Floating a => V3 a
worldUp = V3 0 1 0

-- Directional unit vector of the camera given pitch and yaw
direction :: (Epsilon a, Floating a) => Camera a -> V3 a
direction Camera{..} = eulerDirection camPitch camYaw

right :: Floating a => Camera a -> V3 a
right Camera{..} = V3 (sin camYaw) 0 (cos camYaw)

toViewMatrix :: (Epsilon a, Floating a) => Camera a -> M44 a
toViewMatrix cam@Camera{..} =
  let dir    = direction cam
      -- the 'centre' to which the camera is looking
      centre = camPos + dir
      -- no camera roll so the camera is always on the x-z plane
      up     = right cam `cross` dir -- camera's up
  in lookAt camPos centre up

toInverseViewMatrix :: (Epsilon a, Floating a) => Camera a -> M44 a
toInverseViewMatrix = inv44 . toViewMatrix
