module App.Output (
  Output(..),

  World(..),

  Daylight(..),
  Coin(..),
  Peasant(..)
) where

import Graphics.UI.GLFW
import Linear

import App.Camera

data Output = Output {
  outputConsoleOpen :: Bool,
  outputConsoleText :: String,
  outputDebugKeys :: [(Key, ModifierKeys)],
  outputDebugMouseButtons :: [MouseButton],
  outputDebugQuadOverlay :: Bool,
  outputDebugInfoOverlay :: Bool,
  outputShouldExit :: Bool,
  outputWorld :: World
}

data World = World {
  worldAnimationTime :: Float,
  worldCamera :: Camera,
  worldCoins :: [Coin],
  worldDaylight :: Daylight,
  worldPeasants :: [Peasant],
  worldPlayerCoins :: Int,
  worldPlayerDirection :: V3 Float,
  worldPlayerPosition :: V3 Float,
  worldPlayerVelocity :: V3 Float,
  worldPointerPosition :: V3 Float
}

data Daylight = Daylight {
  daylightAmbientIntensity :: Float,
  -- Pointing at the sun
  daylightPitch :: Float,
  daylightYaw :: Float
}

data Coin = Coin {
  coinTimeDropped :: Float,
  coinPosition :: V3 Float
} deriving (Show)

data Peasant = Peasant {
  peasantDirection :: V3 Float,
  peasantPosition :: V3 Float,
  peasantVelocity :: V3 Float
}
