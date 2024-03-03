module App (
  Env(..),
  MsaaSubsamples(..),
  windowAspectRatio,

  Frame,

  Input(..),
  Time,
  DeltaT,

  Output(..),
  WorldState(..),
  Sun(..),

  PlayerPosition,
  PosX,
  PosZ,

  game
) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Fixed
import Data.List (delete, insert)
import Data.Time.Clock
import Data.Tuple.Extra
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import Reflex
import Reflex.Network

import Camera (Camera(..))
import Cursor
import qualified Camera as Cam

data MsaaSubsamples = MsaaNone | Msaa2x | Msaa4x | Msaa8x | Msaa16x
  deriving (Eq, Enum)

data Env = Env {
  multisampleSubsamples :: MsaaSubsamples,
  consoleDebuggingEnabled :: Bool,
  debugInfoEnabledDefault :: Bool,
  fullscreen :: Bool,
  windowHeight :: Int,
  windowWidth :: Int,
  -- TODO User savable settings
  vsyncEnabled :: Bool
}

type AspectRatio = Float

windowAspectRatio :: Env -> AspectRatio
windowAspectRatio Env {..} = realToFrac windowWidth / realToFrac windowHeight

type Frame = (Input, Output)

data Input = Input {
  cursorPos :: CursorPosition,
  keys :: [(GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)],
  time :: Time,
  deltaT :: DeltaT
} deriving (Show)

type Time = NominalDiffTime
type DeltaT = NominalDiffTime
type HeldKeys = [GLFW.Key]

data Output = Output {
  shouldExit :: Bool,
  shouldOverlayLightDepthQuad :: Bool,
  shouldOverlayDebugInfo :: Bool,
  worldState :: WorldState
}

data Sun = Sun {
  sunPitch :: Float,
  sunYaw :: Float
}

data WorldState = WorldState {
  animationTime :: Float,
  camera :: Camera Float,
  daylightAmbientIntensity :: Float,
  -- | Pointing at the sun
  playerPosition :: PlayerPosition,
  sun :: Sun
}

type PosX = Float
type PosZ = Float
type PlayerPosition = (PosX, PosZ)

playerStart :: Floating a => (a, a)
playerStart = (0, 0)

-- World units per second
playerSpeed :: Floating a => a
playerSpeed = 2

debugCameraSpeed :: Floating a => a
debugCameraSpeed = 6

quitKey :: GLFW.Key
quitKey = GLFW.Key'Escape

toggleDebugQuadKey :: GLFW.Key
toggleDebugQuadKey = GLFW.Key'F2

toggleDebugInfoKey :: GLFW.Key
toggleDebugInfoKey = GLFW.Key'F3

toggleDebugCameraKey :: GLFW.Key
toggleDebugCameraKey = GLFW.Key'Backspace

game :: forall t m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader Env m)
    => Event t Input
    -> m (Dynamic t Frame)
game eInput = do
  Env {..} <- ask
  delta <- holdDyn 0 . fmap deltaT $ eInput
  animationT <- foldDyn ((+) . realToFrac) 0 . fmap deltaT $ eInput
  cursor <- holdDyn (0, 0) . fmap cursorPos $ eInput
  let keyPresses = fmap (fmap fst3 . filter ((== GLFW.KeyState'Pressed) . snd3) . keys) eInput
  heldKeys <- foldDyn (flip (foldl (flip $ uncurry3 updateHeldKeys)) . keys) [] eInput
  shouldExit <- holdDyn False . fmap (const True) . ffilter (elem quitKey)
    $ keyPresses
  overlayQuad <- toggle False . ffilter (elem toggleDebugQuadKey) $ keyPresses
  debugCameraOn <- toggle False . ffilter (elem toggleDebugCameraKey)
    $ keyPresses
  debugInfoOn <- toggle debugInfoEnabledDefault
    . ffilter (elem toggleDebugInfoKey)
    $ keyPresses
  playerPosition <- foldDyn (uncurry . flip $ updatePlayerPosition) playerStart
    . gate (fmap not . current $ debugCameraOn)
    . updated $ (,) <$> heldKeys <*> delta
  let playerCamera = fmap (uncurry playerPositionCamera) playerPosition
  debugCam <- debugCamera delta playerCamera debugCameraOn heldKeys cursor
  let camera = debugCameraOn >>= \d -> if d then debugCam else playerCamera
  sunPitch <- foldDyn (uncurry updateSunPitch) (pi / 2) . updated
    $ (,) <$> delta <*> heldKeys
  let ambientLight = fmap ((* 1) . max 0 . sin) sunPitch
      worldState = WorldState
        <$> animationT
        <*> camera
        <*> ambientLight
        <*> playerPosition
        <*> (Sun <$> sunPitch <*> pure (pi / 8))
  let output = Output
        <$> shouldExit
        <*> overlayQuad
        <*> debugInfoOn
        <*> worldState
  -- Return the current input with the output.
  -- We can use undefined for the initial value because output is never
  -- produced until there has been an actual input.
  input <- holdDyn undefined eInput
  return $ (,) <$> input <*> output
 where
  updateHeldKeys :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> [GLFW.Key] -> [GLFW.Key]
  updateHeldKeys k GLFW.KeyState'Pressed  _ ks = insert k ks
  updateHeldKeys k GLFW.KeyState'Released _ ks = delete k ks
  updateHeldKeys _ _                      _ ks = ks

  playerPositionCamera :: Floating a => a -> a -> Camera a
  playerPositionCamera x z =
    let th = (3 * pi) / 8
        h  = 30
    in Camera {
         camPitch = negate th,
         camPos = L.V3 x h (z + h / tan th),
         camYaw = pi / 2
       }

  updateSunPitch :: DeltaT -> HeldKeys -> Float -> Float
  updateSunPitch dt keys pitch = (+ pitch) . sum . fmap keyChange $ keys
   where
    keyChange GLFW.Key'Equal = angularVelocity * realToFrac dt
    keyChange GLFW.Key'Minus = negate angularVelocity * realToFrac dt
    keyChange _              = 0
    -- Radians per second
    angularVelocity = 1

  updatePlayerPosition :: DeltaT -> HeldKeys -> PlayerPosition -> PlayerPosition
  updatePlayerPosition dt keys (x, z) =
    let (L.V3 dx _ dz) = (playerSpeed *) . (realToFrac dt *) . sum
                           . fmap keyVelocity $ keys
    in (x + dx, z + dz)
   where
    keyVelocity :: Floating a => GLFW.Key -> L.V3 a
    keyVelocity GLFW.Key'W = L.V3   0   0 (-1)
    keyVelocity GLFW.Key'A = L.V3 (-1)  0   0
    keyVelocity GLFW.Key'S = L.V3   0   0   1
    keyVelocity GLFW.Key'D = L.V3   1   0   0
    keyVelocity _          = L.V3   0   0   0

  debugCamera :: forall a. (Floating a, Real a, L.Epsilon a)
    => Dynamic t DeltaT
    -> Dynamic t (Camera a)
    -> Dynamic t Bool
    -> Dynamic t HeldKeys
    -> Dynamic t CursorPosition
    -> m (Dynamic t (Camera a))
  debugCamera delta playerCamera debugCameraOn heldKeys cursor = do
    -- Reset the debug camera to the last player camera position every time
    -- its toggled on.
    let camOn = attachPromptlyDyn playerCamera . updated $ debugCameraOn
    pos <- networkHold (return playerCamera)
             . fmap (uncurry debugCameraPosition) $ camOn
    return . join $ pos
   where
    debugCameraPosition :: Camera a
      -> Bool
      -> m (Dynamic t (Camera a))
    debugCameraPosition camStart camOn = do
      s <- foldDyn (uncurry3 updateDebugCamera) ((0, 0), camStart)
       . gate (pure camOn)
       . updated
       $ (,,) <$> delta <*> heldKeys <*> cursor
      return $ fmap snd s

    updateDebugCamera :: DeltaT
      -> HeldKeys
      -> CursorPosition
      -> (CursorPosition, Camera a)
      -> (CursorPosition, Camera a)
    updateDebugCamera deltaT keys (x', y') ((x, y), camera@Camera{..}) =
      let velocity = pure debugCameraSpeed * realToFrac deltaT
                       * (sum . map keyVelocity $ keys)
          position = camPos + velocity
          -- Delta pitch and yaw are the negation of the change in cursor
          -- position according to the GLFW window.
          pitch = max (-pi / 2) . min (pi / 2)
                    . (+ realToFrac (y - y') * cameraMouseSensitivity)
                    $ camPitch
          yaw = (`mod'` (2 * pi)) . (+ camYaw) . (* cameraMouseSensitivity)
                  . realToFrac $ (x - x')
          camera' = camera {
            camPos   = position,
            camPitch = pitch,
            camYaw   = yaw
          }
      in ((x', y'), camera')
     where
      -- TODO Move this somewhere easier to find
      cameraMouseSensitivity = 0.0015

      keyVelocity :: GLFW.Key -> L.V3 a
      keyVelocity GLFW.Key'W = Cam.direction camera
      keyVelocity GLFW.Key'A = negate . Cam.right $ camera
      keyVelocity GLFW.Key'S = negate . Cam.direction $ camera
      keyVelocity GLFW.Key'D = Cam.right camera
      keyVelocity _          = L.V3 0 0 0
