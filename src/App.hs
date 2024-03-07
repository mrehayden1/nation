module App (
  Env(..),
  MsaaSubsamples(..),

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
import Linear
import Reflex
import Reflex.Network

import Camera
import qualified Camera as Cam
import Cursor
import Matrix

data MsaaSubsamples = MsaaNone | Msaa2x | Msaa4x | Msaa8x | Msaa16x
  deriving (Eq, Enum)

data Env = Env {
  multisampleSubsamples :: MsaaSubsamples,
  consoleDebuggingEnabled :: Bool,
  debugInfoEnabledDefault :: Bool,
  fullscreen :: Bool,
  -- TODO User savable settings
  vsyncEnabled :: Bool,
  windowHeight :: Int,
  windowWidth :: Int
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

data Sun = Sun {
  sunPitch :: Float,
  sunYaw :: Float
}

data WorldState = WorldState {
  animationTime :: Float,
  camera :: Camera,
  daylightAmbientIntensity :: Float,
  -- | Pointing at the sun
  playerPosition :: PlayerPosition,
  pointerPosition :: V3 Float,
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

type App t a = forall m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader Env m) => m a

game :: forall t. Event t Input -> App t (Dynamic t Frame)
game eInput = do
  Env{..} <- ask
  delta <- holdDyn 0 . fmap deltaT $ eInput
  animationT <- foldDyn ((+) . realToFrac) 0 . fmap deltaT $ eInput
  let cursorE = fmap cursorPos eInput
  cursor <- holdDyn (0, 0) cursorE
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
    . gate (fmap not . current $ debugCameraOn) . updated
    $ (,) <$> heldKeys <*> delta
  let playerCamera = fmap (uncurry playerPositionCamera) playerPosition
  debugCam <- debugCamera delta playerCamera debugCameraOn heldKeys cursor
  let camera = debugCameraOn >>= \d -> if d then debugCam else playerCamera
  sunPitch <- foldDyn (uncurry updateSunPitch) (pi / 2) . updated
    $ (,) <$> delta <*> heldKeys
  pointer <- makePointer cursorE camera
  let ambientLight = fmap ((* 1) . max 0 . sin) sunPitch
      worldState = WorldState
        <$> animationT
        <*> camera
        <*> ambientLight
        <*> playerPosition
        <*> pointer
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

  playerPositionCamera :: Float -> Float -> Camera
  playerPositionCamera x z =
    let th = (3 * pi) / 8
        h  = 40
    in Camera {
         camPitch = negate th,
         -- always look at the player position
         camPos = V3 x h (z + h / tan th),
         -- towards -z
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
    let (V3 dx _ dz) = (playerSpeed *) . (realToFrac dt *) . sum
                           . fmap keyVelocity $ keys
    in (x + dx, z + dz)
   where
    keyVelocity :: Floating a => GLFW.Key -> V3 a
    keyVelocity GLFW.Key'W = V3   0   0 (-1)
    keyVelocity GLFW.Key'A = V3 (-1)  0   0
    keyVelocity GLFW.Key'S = V3   0   0   1
    keyVelocity GLFW.Key'D = V3   1   0   0
    keyVelocity _          = V3   0   0   0

  debugCamera :: Dynamic t DeltaT
    -> Dynamic t Camera
    -> Dynamic t Bool
    -> Dynamic t HeldKeys
    -> Dynamic t CursorPosition
    -> App t (Dynamic t Camera)
  debugCamera delta playerCamera debugCameraOn heldKeys cursor = do
    -- Reset the debug camera to the last player camera position every time
    -- its toggled on.
    let camOn = attachPromptlyDyn playerCamera . updated $ debugCameraOn
    pos <- networkHold (return playerCamera)
             . fmap (uncurry debugCameraPosition) $ camOn
    return . join $ pos
   where
    debugCameraPosition :: Camera
      -> Bool
      -> App t (Dynamic t Camera)
    debugCameraPosition camStart camOn = do
      s <- foldDyn (uncurry3 updateDebugCamera) ((0, 0), camStart)
       . gate (pure camOn)
       . updated
       $ (,,) <$> delta <*> heldKeys <*> cursor
      return $ fmap snd s

    updateDebugCamera :: DeltaT
      -> HeldKeys
      -> CursorPosition
      -> (CursorPosition, Camera)
      -> (CursorPosition, Camera)
    updateDebugCamera deltaT keys (x', y') ((x, y), camera@Camera{..}) =
      let velocity = pure debugCameraSpeed * realToFrac deltaT
                       * (sum . map keyVelocity $ keys)
          position = camPos + velocity
          -- Delta pitch and yaw are the negation of the change in cursor
          -- position according to GLFW window co-ordinates.
          pitch = max (-pi / 2) . min (pi / 2)
                    . (+ (y - y') * cameraMouseSensitivity)
                    $ camPitch
          yaw = (`mod'` (2 * pi)) . (+ camYaw) . (* cameraMouseSensitivity)
                  $ (x - x')
          camera' = camera {
            camPos   = position,
            camPitch = pitch,
            camYaw   = yaw
          }
      in ((x', y'), camera')
     where
      -- TODO Move this somewhere easier to find
      cameraMouseSensitivity = 0.0015

      keyVelocity :: GLFW.Key -> V3 Float
      keyVelocity GLFW.Key'W = Cam.direction camera
      keyVelocity GLFW.Key'A = negate . Cam.right $ camera
      keyVelocity GLFW.Key'S = negate . Cam.direction $ camera
      keyVelocity GLFW.Key'D = Cam.right camera
      keyVelocity _          = V3 0 0 0

makePointer :: Event t CursorPosition -> Dynamic t Camera -> App t (Dynamic t (V3 Float))
makePointer cursorE cameraD = do
  Env{..} <- ask
  cursorDelta <- fmap (fmap (uncurry $ flip (-)))
                   . foldDyn (flip $ (,) . snd) (0, 0)
                   . fmap (uncurry V2) $ cursorE
  fmap (fmap snd)
    . foldDyn (updatePointer windowWidth windowHeight) (0, 0)
    . updated $ ((,) <$> cursorDelta <*> cameraD)
 where
  -- a pointer which remains within the bounds of a circle centered around the
  -- middle of the player
  updatePointer :: Int
    -> Int
    -> (V2 Float, Camera)
    -> (V2 Float, V3 Float)
    -> (V2 Float, V3 Float)
  updatePointer w h (delta, camera) (screenPos, _) =
    let screenPos' = screenPos + delta
        p@(V3 x _ z) = worldPosition w h camera screenPos'
    in if x**2 + z**2 <= r**2
         then (screenPos', p)
         else let p' = normalize p * pure r
              -- project the new pointer position back into screen space and
              -- use that to calculate future updates
              in (screenPosition w h camera p', p')
   where
    r = 20

  -- The game pointer world coordinates on the plane y = 0
  worldPosition :: Int -> Int -> Camera -> V2 Float -> V3 Float
  worldPosition screenWidth screenHeight camera (V2 cursorX cursorY) =
    let w = realToFrac screenWidth
        h = realToFrac screenHeight
        persM' = inversePerspectiveProjection $ w / h
        viewM' = inv44 . toViewMatrix $ camera
        xNdc =  toNdc w cursorX
        yNdc = -toNdc h cursorY
        V4 viewX viewY _ _ = persM' !* V4 xNdc yNdc (-1) 1
        V4 v1 v2 v3 _ = viewM' !* V4 viewX viewY (-1) 0
        V3 a b c = camPos camera
        -- Using the vector equation of the line `(x,y,z) = (a,b,c) + tv` we
        -- solve t for y = 0 and calculate x and z
        t = -b / v2
        x = a + t * v1
        z = c + t * v3
    in V3 x 0 z
   where
    toNdc :: Float -> Float -> Float
    toNdc d a = 2 * (a - (d/2)) / d

  screenPosition :: Int -> Int -> Camera -> V3 Float -> V2 Float
  screenPosition screenWidth screenHeight camera pos =
    let width = realToFrac screenWidth
        height = realToFrac screenHeight
        persM = perspectiveProjection $ width / height
        viewM = toViewMatrix camera
        V4 x y _ w = persM !*! viewM !* point pos
    in (V2 width height * (V2 (x / w) (-y / w) + 1)) / 2
