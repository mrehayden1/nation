module App (
  Time,
  DeltaT,

  Output(..),
  Input(..),
  WorldState(..),

  app
) where

import Control.Monad
import Control.Monad.Fix
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

type Time = NominalDiffTime
type DeltaT = NominalDiffTime
type HeldKeys = [GLFW.Key]

data Input = Input {
  cursorPos :: CursorPosition,
  keys :: [(GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)],
  time :: Time,
  deltaT :: DeltaT
}

data Output = Output {
  shouldExit :: Bool,
  shouldOverlayLightDepthQuad :: Bool,
  worldState :: WorldState
}

type PosX = Float
type PosZ = Float
type PlayerPosition = (PosX, PosZ)

data WorldState = WorldState {
  camera :: Camera Float,
  daylightAmbientIntensity :: Float,
  daylightDirection :: L.V3 Float
}

defaultCamera :: Floating a => Camera a
defaultCamera = Cam.Camera {
  Cam.pitch = negate $ (3 * pi) / 8,
  Cam.position = L.V3 0 10 3,
  Cam.yaw = negate (pi / 2)
}

-- World units per second
playerSpeed :: Floating a => a
playerSpeed = 2

debugCameraSpeed :: Floating a => a
debugCameraSpeed = 6

quitKey :: GLFW.Key
quitKey = GLFW.Key'Escape

toggleDebugQuadKey :: GLFW.Key
toggleDebugQuadKey = GLFW.Key'F2

toggleDebugCameraKey :: GLFW.Key
toggleDebugCameraKey = GLFW.Key'Backspace

app :: forall t m. (Adjustable t m, MonadFix m, MonadHold t m)
  => Event t Input
  -> m (Dynamic t Output)
app input = do
  delta <- holdDyn 0 . fmap deltaT $ input
  cursor <- holdDyn (0, 0) . fmap cursorPos $ input
  let keyPresses = fmap (fmap fst3 . filter ((== GLFW.KeyState'Pressed) . snd3) . keys) input
  heldKeys <- foldDyn (flip (foldl (flip $ uncurry3 updateHeldKeys)) . keys) [] input
  shouldExit <- holdDyn False . fmap (const True) . ffilter (elem quitKey)
    $ keyPresses
  overlayQuad <- toggle False . ffilter (elem toggleDebugQuadKey) $ keyPresses
  debugCameraOn <- toggle False . ffilter (elem toggleDebugCameraKey)
    $ keyPresses
  playerPosition <- foldDyn (uncurry . flip $ updatePlayerPosition) (0, 0)
    . gate (fmap not . current $ debugCameraOn)
    . updated $ (,) <$> heldKeys <*> delta
  let playerCamera = fmap (uncurry playerPositionCamera) playerPosition
  debugCam <- debugCamera delta playerCamera debugCameraOn heldKeys cursor
  let camera = debugCameraOn >>= \d -> if d then debugCam else playerCamera
  ambientLight <- holdDyn 0
    . fmap ((* 0.1) . max 0 . sin . realToFrac . time)
    $ input
  sunPitch <- foldDyn updateLightDirection (pi / 2) . updated $ heldKeys
  let sunDirection = fmap lightDirection sunPitch
      worldState = WorldState
        <$> camera
        <*> ambientLight
        <*> sunDirection
  {-
      sunDirection = lightDirection $ (3 * pi) / 4
      worldState = WorldState <$> camera <*> pure ambientLight <*> pure sunDirection
  -}
  return $ Output
    <$> shouldExit
    <*> overlayQuad
    <*> worldState
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

  updateLightDirection :: HeldKeys -> Float -> Float
  updateLightDirection keys direction = (+ direction) . sum
    . fmap keyChange $ keys

   where
    keyChange GLFW.Key'Equal = delta
    keyChange GLFW.Key'Minus = negate delta
    keyChange _              = 0

    delta = 0.02

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
