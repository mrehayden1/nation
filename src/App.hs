module App (
  Input(..),
  WorldState(..),

  app
) where

import Control.Lens
import Control.Monad.Fix
import Data.Fixed
import Data.List (delete, insert)
import Data.Time.Clock
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import Reflex

import Camera (Camera)
import Cursor
import qualified Camera as Cam

type ReflexApp t m = (Reflex t, MonadHold t m, MonadFix m)
       => Event t Input
       -> m (Behavior t (WorldState, ShouldExit, OverlayQuad))

type OverlayQuad = Bool
type ShouldExit = Bool

type Time = NominalDiffTime
type Delta = NominalDiffTime

data Input = Input {
  mousePosition :: (GLFWMouseX, GLFWMouseY),
  keyEvents :: [(GLFW.Key, GLFW.KeyState)],
  time :: Time,
  deltaT :: Delta
}

data WorldState = WorldState {
  camera :: Camera Float,
  daylightAmbientIntensity :: Float,
  daylightDirection :: L.V3 Float
}

defaultCamera :: Floating a => Camera a
defaultCamera = Cam.Camera {
  Cam.pitch = 0,
  Cam.position = L.V3 0 0 3,
  Cam.yaw = negate (pi / 2)
}

-- Radians per pixel
cameraMouseSensitivity :: Floating a => a
cameraMouseSensitivity = 0.0015

-- World units per second
cameraSpeed :: Floating a => a
cameraSpeed = 2

app :: ReflexApp t m
app input = do
  heldKeys <- foldDyn (flip $ foldl (flip $ uncurry updateHeldKeys)) []
    . fmap keyEvents $ input
  let pressedKeys =
        fmap (fmap fst . filter (((||) <$> (== GLFW.KeyState'Pressed) <*> (== GLFW.KeyState'Repeating)) . snd) . keyEvents) input
  shouldExit <-
    foldDyn (\ks quit -> quit || elem GLFW.Key'Escape ks) False pressedKeys
  overlayQuad <- holdDyn False . fmap (elem GLFW.Key'Q) . updated $ heldKeys 
  camera <- fmap (fmap (\(_, cam, _) -> cam))
    . foldDyn updateCamera (defaultCursorPosition, defaultCamera, [])
    $ input
  {-
  ambientLight <- holdDyn 0
    . fmap ((+ 0.05) . (* 0.1) . max 0 . sin . realToFrac . time)
    $ input
  -}
  sunPitch <- foldDyn updateLightDirection (pi / 2) pressedKeys
  let sunDirection = fmap lightDirection sunPitch
      ambientLight = 0.1
      worldState = WorldState <$> camera <*> pure ambientLight <*> sunDirection
  {-
      sunDirection = lightDirection $ (3 * pi) / 4
      worldState = WorldState <$> camera <*> pure ambientLight <*> pure sunDirection
  -}
  return . current $ (,,) <$> worldState <*> shouldExit <*> overlayQuad
 where 
  lightDirection pitch = 
    let yaw = pi / 8
        lx = cos yaw * cos pitch
        ly = sin pitch
        lz = sin yaw * cos pitch
    in L.normalize $ L.V3 lx ly lz

  updateLightDirection :: [GLFW.Key] -> Float -> Float
  updateLightDirection keys direction = (+ direction) . sum
    . fmap keyChange $ keys

   where
    keyChange GLFW.Key'Equal = delta
    keyChange GLFW.Key'Minus = negate delta
    keyChange _              = 0

    delta = 0.02

  updateCamera :: (L.Epsilon a, Floating a, Ord a)
    => Input
    -> (CursorPosition, Camera a, [GLFW.Key])
    -> (CursorPosition, Camera a, [GLFW.Key])
  updateCamera Input{..} ((x, y), camera, keys) =
    let (x', y') = mousePosition
        keys' = foldl (flip $ uncurry updateHeldKeys) keys keyEvents
        direction = Cam.direction camera
        velocity = pure cameraSpeed * realToFrac deltaT
                     * (sum . map (keyVelocity direction Cam.up) $ keys)
        position = Cam.position camera + velocity
        -- negate the delta in pitch since a GLFW window uses an inverted y-axis
        pitch = max (negate $ pi / 2) . min (pi / 2) $ Cam.pitch camera
                  + realToFrac (y - y') * cameraMouseSensitivity
        yaw = Cam.yaw camera
                + realToFrac (x' - x) * cameraMouseSensitivity
        camera' = camera {
          Cam.position = position,
          Cam.pitch    = pitch,
          Cam.yaw      = yaw
        }
    in ((x', y'), camera', keys')
   where
    keyVelocity :: (L.Epsilon a, Floating a)
      => L.V3 a
      -> L.V3 a
      -> GLFW.Key
      -> L.V3 a
    keyVelocity direction  _  GLFW.Key'W = L.normalize . set L._y 0 $ direction
    keyVelocity direction  up GLFW.Key'A = L.normalize . L.cross up $ direction
    keyVelocity direction  _  GLFW.Key'S = negate . L.normalize
      . set L._y 0 $ direction
    keyVelocity direction  up GLFW.Key'D = L.normalize . L.cross direction $ up
    keyVelocity _          _  _          = L.V3 0 0 0

  updateHeldKeys :: GLFW.Key -> GLFW.KeyState -> [GLFW.Key] -> [GLFW.Key]
  updateHeldKeys k GLFW.KeyState'Pressed  ks = insert k ks
  updateHeldKeys k GLFW.KeyState'Released ks = delete k ks
  updateHeldKeys _ _                      ks = ks
