module App (
  Env(..),

  Frame,

  Input(..),
  Time,
  DeltaT,

  Output(..),
  WorldState(..),

  PlayerPosition,
  PosX,
  PosZ,

  game
) where

import Control.Monad
import Control.Monad.Reader
import Data.List (delete, insert)
import Data.Time.Clock
import Data.Tuple.Extra
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import Reflex
import Reflex.Network

import Camera (Camera)
import Cursor
import qualified Camera as Cam

data Env = Env {
  consoleDebuggingEnabled :: Bool,
  debugInfoEnabledDefault :: Bool,
  windowHeight :: Int,
  windowWidth :: Int,
  -- TODO User savable settings
  vsyncEnabled :: Bool
}

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

data WorldState = WorldState {
  daylightAmbientIntensity :: Float,
  daylightDirection :: L.V3 Float,
  camera :: Camera Float,
  playerPosition :: PlayerPosition
}

type PosX = Float
type PosZ = Float
type PlayerPosition = (PosX, PosZ)

defaultCamera :: Floating a => Camera a
defaultCamera = Cam.Camera {
  Cam.pitch = negate $ (3 * pi) / 8,
  Cam.position = L.V3 0 10 3,
  Cam.yaw = negate $ pi / 2
}

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
  ambientLight <- holdDyn 0
    . fmap ((* 0.1) . max 0 . sin . realToFrac . time)
    $ eInput
  sunPitch <- foldDyn (uncurry updateLightDirection) (pi / 2) . updated
    $ (,) <$> delta <*> heldKeys
  let sunDirection = fmap lightDirection sunPitch
      worldState = WorldState
        <$> ambientLight
        <*> sunDirection
        <*> camera
        <*> playerPosition
  {-
      sunDirection = lightDirection $ (3 * pi) / 4
      worldState = WorldState <$> camera <*> pure ambientLight <*> pure sunDirection
  -}
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
    let position = Cam.position defaultCamera
    in defaultCamera { Cam.position = position + L.V3 x 0 z }

  lightDirection :: (L.Epsilon a, Floating a) => a -> L.V3 a
  lightDirection pitch = 
    let yaw = pi / 8
        lx = cos yaw * cos pitch
        ly = sin pitch
        lz = sin yaw * cos pitch
    in L.normalize $ L.V3 lx ly lz

  updateLightDirection :: DeltaT -> HeldKeys -> Float -> Float
  updateLightDirection deltaT keys direction = (+ direction) . sum
    . fmap keyChange $ keys

   where
    keyChange GLFW.Key'Equal = angularVelocity * realToFrac deltaT
    keyChange GLFW.Key'Minus = negate angularVelocity * realToFrac deltaT
    keyChange _              = 0

    -- Radians per second
    angularVelocity = 1

  updatePlayerPosition :: DeltaT -> HeldKeys -> PlayerPosition -> PlayerPosition
  updatePlayerPosition dT keys (x, z) =
    let (L.V3 x' _ z') = (playerSpeed *) . (realToFrac dT *) . sum . fmap keyVelocity $ keys
    in (x + x', z + z')
   where
    keyVelocity :: Floating a => GLFW.Key -> L.V3 a
    keyVelocity GLFW.Key'W = L.V3   0   0 (-1)
    keyVelocity GLFW.Key'A = L.V3 (-1)  0   0
    keyVelocity GLFW.Key'S = L.V3   0   0   1
    keyVelocity GLFW.Key'D = L.V3   1   0   0
    keyVelocity _          = L.V3   0   0   0

  debugCamera :: forall a. (Floating a, L.Epsilon a, Ord a)
    => Dynamic t DeltaT
    -> Dynamic t (Camera a)
    -> Dynamic t Bool
    -> Dynamic t HeldKeys
    -> Dynamic t CursorPosition
    -> m (Dynamic t (Camera a))
  debugCamera delta playerCamera debugCameraOn heldKeys cursor = do
    --debugCameraPosition defaultCamera False
    -- Set the debug camera to the last player camera position every time its
    -- toggled on.
    let camOn = attachPromptlyDyn playerCamera . updated $ debugCameraOn
    pos <- networkHold (return . pure $ defaultCamera)
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
    updateDebugCamera deltaT keys (x', y') ((x, y), camera) =
      let direction = Cam.direction camera
          velocity = pure debugCameraSpeed * realToFrac deltaT
                       * (sum . map (keyVelocity direction Cam.up) $ keys)
          position = Cam.position camera + velocity
          -- Negate the delta in pitch since a GLFW window uses an inverted
          -- y-axis relative to OpenGL.
          pitch = max (negate (pi / 2) + 0.1)
                    . min (pi / 2)
                    . (+ realToFrac (y - y') * cameraMouseSensitivity)
                    $ Cam.pitch camera

          yaw = Cam.yaw camera
                  + realToFrac (x' - x) * cameraMouseSensitivity
          camera' = camera {
            Cam.position = position,
            Cam.pitch    = pitch,
            Cam.yaw      = yaw
          }
      in ((x', y'), camera')
     where
      -- TODO Move this somewhere easier to find
      cameraMouseSensitivity = 0.0015

      keyVelocity :: L.V3 a
        -> L.V3 a
        -> GLFW.Key
        -> L.V3 a
      keyVelocity direction  _  GLFW.Key'W = L.normalize direction
      keyVelocity direction  up GLFW.Key'A = L.normalize . L.cross up $ direction
      keyVelocity direction  _  GLFW.Key'S = negate . L.normalize $ direction
      keyVelocity direction  up GLFW.Key'D = L.normalize . L.cross direction $ up
      keyVelocity _          _  _          = L.V3 0 0 0
