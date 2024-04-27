module App.Render.Scene.Scene (
  Scene(..),
  Daylight(..),
  Element(..)
) where

import Data.Text
import Linear as L

import App.Camera as Cam
import App.Render.Model

data Scene = Scene {
  sceneCamera :: Camera,
  sceneElements :: [Element],
  sceneDaylight :: Daylight
}

data Daylight = Daylight {
  daylightAmbientIntensity :: Float,
  daylightPitch :: Float,
  daylightYaw :: Float
}

data Element = Element {
  elementAnimation :: Maybe Animation,
  elementModel :: Model,
  elementPosition :: V3 Float,
  elementRotation :: Quaternion Float,
  elementShadow :: Bool
}

type Animation = (AnimationName, AnimationDuration, AnimationTime)
type AnimationName = Text
type AnimationTime = Float
type AnimationDuration = Float
