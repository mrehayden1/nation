module App.Render.Scene.Scene (
  Scene(..),

  Daylight(..),

  Instance(..),
  ModelMatrix,
  JointMatrix,
) where

import Data.Map
import Data.Vector
import Linear as L

import App.Camera as Cam
import App.Entity

data Scene = Scene {
  sceneCamera :: Camera Float,
  sceneDaylight :: Daylight,
  sceneEntities :: Map Entity [Instance]
} deriving (Show)

data Instance = Instance {
  instanceModelMatrix :: ModelMatrix,
  -- Global transformation matrices indexed by node. Transforms un-skinned
  -- mesh vertices from joint space to (posed, if animated) model space.
  instanceTransformationMatrices :: Vector TransformationMatrix,
  -- Indexed by skin id and then joint id (defined in the skin's top-level
  -- object). Transforms skinned mesh vertices from bind space to final model
  -- space.
  instanceJointMatrices :: Vector (Vector JointMatrix)
} deriving (Show)

type ModelMatrix = M44 Float
type TransformationMatrix = M44 Float
type JointMatrix = M44 Float

data Daylight = Daylight {
  daylightColor :: V3 Float,
  daylightIntensity :: Float,
  daylightPitch :: Float,
  daylightYaw :: Float
} deriving (Show)
