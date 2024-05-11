module App.Render.Scene.Scene (
  Scene(..),
  Daylight(..),
  Element(..)
) where

import Data.Text
import Linear as L

import App.Camera as Cam
import App.Entity.Collision3D
import App.Render.Model

data Scene = Scene {
  sceneCamera :: Camera Float,
  sceneElements :: [Element],
  sceneDaylight :: Daylight
} deriving (Show)

data Daylight = Daylight {
  daylightColor :: V3 Float,
  daylightIntensity :: Float,
  daylightPitch :: Float,
  daylightYaw :: Float
} deriving (Show)

data Element = Element {
  elementAnimation :: Maybe Animation,
  elementCullingBounds :: Maybe (Collision3D Float),
  elementModel :: Model,
  elementPosition :: V3 Float,
  elementRotation :: Quaternion Float,
  elementShadow :: Bool
} deriving (Show)

type Animation = (AnimationName, AnimationDuration, AnimationTime)
type AnimationName = Text
type AnimationTime = Float
type AnimationDuration = Float
