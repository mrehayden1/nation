module App (
  Env(..),
  MsaaSubsamples(..),

  Frame,

  Input(..),
  Time,
  DeltaT,

  Output(..),
  World(..),
  Daylight(..),

  PlayerPosition,
  PosX,
  PosZ,

  game
) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Fixed
import Data.Functor
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
import Quaternion

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
  inputCursorPos :: CursorPosition,
  inputMouseButtons :: [(GLFW.MouseButton, GLFW.MouseButtonState, GLFW.ModifierKeys)],
  inputKeys :: [(GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)],
  inputTime :: Time,
  inputDeltaT :: DeltaT
} deriving (Show)

buttonMousePrimary :: GLFW.MouseButton
buttonMousePrimary = GLFW.MouseButton'1

keyExit :: GLFW.Key
keyExit = GLFW.Key'Escape

keyDebugInfoToggle :: GLFW.Key
keyDebugInfoToggle = GLFW.Key'F3

keyDebugQuadToggle :: GLFW.Key
keyDebugQuadToggle = GLFW.Key'F2

keyConsoleToggle :: GLFW.Key
keyConsoleToggle = GLFW.Key'GraveAccent

keyDebugCameraToggle :: GLFW.Key
keyDebugCameraToggle = GLFW.Key'Backspace

type Time = NominalDiffTime
type DeltaT = NominalDiffTime

data Output = Output {
  outputConsoleOpen :: Bool,
  outputConsoleText :: String,
  outputDebugKeys :: [(GLFW.Key, GLFW.ModifierKeys)],
  outputDebugMouseButtons :: [GLFW.MouseButton],
  outputDebugQuadOverlay :: Bool,
  outputDebugInfoOverlay :: Bool,
  outputShouldExit :: Bool,
  outputWorld :: World
}

data Daylight = Daylight {
  daylightAmbientIntensity :: Float,
  -- Pointing at the sun
  daylightSunPitch :: Float,
  daylightSunYaw :: Float
}

data World = World {
  worldAnimationTime :: Float,
  worldCamera :: Camera,
  worldDaylight :: Daylight,
  worldPlayerDirection :: Quaternion Float,
  worldPlayerPosition :: V3 Float,
  worldPointerPosition :: V3 Float
}

type PosX = Float
type PosZ = Float
type PlayerPosition = (PosX, PosZ)

playerStart :: Floating a => V3 a
playerStart = V3 0 0 0

-- Meters per second
playerSpeed :: Floating a => a
playerSpeed = 4.5

debugCameraSpeed :: Floating a => a
debugCameraSpeed = 6

type App t a = forall m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader Env m) => m a

game :: forall t. Event t Input -> App t (Dynamic t Frame)
game eInput = do
  Env{..} <- ask
  deltaT <- holdDyn 0 . fmap inputDeltaT $ eInput
  animationT <- foldDyn ((+) . realToFrac) 0 . fmap inputDeltaT $ eInput
  -- Keys
  (keyPresses, heldKeys) <- keys eInput
  -- Mouse buttons
  (buttonPresses, heldButtons) <- buttons eInput
  let clicks = ffilter (elem buttonMousePrimary) buttonPresses
  -- Cursor window position
  let cursorE = fmap inputCursorPos eInput
  cursor <- holdDyn (0, 0) cursorE
  -- Console
  consoleOpen <- toggle False . ffilter (elem keyConsoleToggle . fmap fst)
    $ keyPresses
  consoleText <- console consoleOpen keyPresses
  -- Shortcuts
  shouldExit <- holdDyn False . ($> True) . ffilter (elem keyExit . fmap fst)
    $ keyPresses
  overlayDebugQuad <- toggle False
    . ffilter (elem keyDebugQuadToggle . fmap fst) $ keyPresses
  overlayDebugInfo <- toggle debugInfoEnabledDefault
    . ffilter (elem keyDebugInfoToggle . fmap fst) $ keyPresses
  debugCameraOn <- toggle False
    . ffilter (elem keyDebugCameraToggle . fmap fst)
    . gate (current . fmap not $ consoleOpen) $ keyPresses
  -- Player position
  rec
    moveSelection <- holdDyn playerStart . tag (current pointerD) $ clicks
    let playerDirection = (normalize .) . (-)
                            <$> moveSelection <*> playerPosition
    playerPosition <- foldDyn (+) playerStart
      . attachWith
          (\a b -> a ^* playerSpeed * realToFrac b)
          (current playerDirection)
      . fmap inputDeltaT
      $ eInput
  -- Player camera
    let playerCamera = pure $ setPlayerCamera playerStart
  -- Pointer
    pointerD <- pointer cursorE playerCamera
  -- Debug camera
  debugCam <- debugCamera deltaT playerCamera debugCameraOn heldKeys cursor
  let camera = debugCameraOn >>= \d -> if d then debugCam else playerCamera
  -- Lighting
  sunPitch <- foldDyn (uncurry updateSunPitch) (pi / 2) . updated
    $ (,) <$> deltaT <*> heldKeys
  let ambientLight = fmap ((* 1) . max 0 . sin) sunPitch
  -- Output
      worldState = World
        <$> animationT
        <*> camera
        <*> (Daylight <$> ambientLight <*> sunPitch <*> pure (pi / 8))
        <*> fmap (fromVectors (V3 1 0 0)) playerDirection
        <*> playerPosition
        <*> pointerD
      output = Output
        <$> consoleOpen
        <*> consoleText
        <*> heldKeys
        <*> heldButtons
        <*> overlayDebugQuad
        <*> overlayDebugInfo
        <*> shouldExit
        <*> worldState
  -- Return the current input with the output.
  -- We can use undefined for the initial value because output is never
  -- produced until there has been an input.
  input <- holdDyn undefined eInput
  return $ (,) <$> input <*> output
 where
  setPlayerCamera :: V3 Float -> Camera
  setPlayerCamera (V3 x _ z) =
    let th = (3 * pi) / 8
        h  = 40
    in Camera {
         camPitch = negate th,
         -- always look at the player position
         camPos = V3 x h (z + h / tan th),
         -- towards -z
         camYaw = pi / 2
       }

  updateSunPitch :: DeltaT -> [(GLFW.Key, GLFW.ModifierKeys)] -> Float -> Float
  updateSunPitch dt ks pitch = (+ pitch) . sum . fmap keyChange $ ks
   where
    keyChange (GLFW.Key'Equal, _) = angularVelocity * realToFrac dt
    keyChange (GLFW.Key'Minus, _) = negate angularVelocity * realToFrac dt
    keyChange _                   = 0
    -- Radians per second
    angularVelocity = 1

  updatePlayerPosition :: DeltaT -> [GLFW.Key] -> PlayerPosition -> PlayerPosition
  updatePlayerPosition dt ks (x, z) =
    let (V3 dx _ dz) = (playerSpeed *) . (realToFrac dt *) . sum
                           . fmap keyVelocity $ ks
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
    -> Dynamic t [(GLFW.Key, GLFW.ModifierKeys)]
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
       $ (,,) <$> delta <*> fmap (fmap fst) heldKeys <*> cursor
      return $ fmap snd s

    updateDebugCamera :: DeltaT
      -> [GLFW.Key]
      -> CursorPosition
      -> (CursorPosition, Camera)
      -> (CursorPosition, Camera)
    updateDebugCamera deltaT ks (x', y') ((x, y), camera@Camera{..}) =
      let velocity = pure debugCameraSpeed * realToFrac deltaT
                       * (sum . map keyVelocity $ ks)
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

console :: Dynamic t Bool
  -> Event t [(GLFW.Key, GLFW.ModifierKeys)]
  -> App t (Dynamic t String)
console open = foldDyn (flip (foldl updateText)) "" . gate (current open)
 where
  updateText :: String -> (GLFW.Key, GLFW.ModifierKeys) -> String
  updateText s (GLFW.Key'Backspace, _) = take (length s - 1) s
  updateText s (alpha             , _)
    | alpha >= GLFW.Key'A && alpha <= GLFW.Key'Z = s ++ (drop 4 . show $ alpha)
  updateText s _                       = s

keys :: Event t Input
  -> App t (Event t [(GLFW.Key, GLFW.ModifierKeys)]  ,
            Dynamic t [(GLFW.Key, GLFW.ModifierKeys)])
keys eInput = do
  let pressed = ffilter (not . null) . fmap filterPressed $ eInput
  held <- foldDyn
    (flip (foldl (flip $ uncurry3 updateHeldKeys)) . inputKeys)
    []
    eInput
  return (pressed, held)
 where
  filterPressed = fmap ((,) <$> fst3 <*> thd3)
   . filter (flip elem [GLFW.KeyState'Pressed, GLFW.KeyState'Repeating] . snd3)
   . inputKeys

  updateHeldKeys :: GLFW.Key
    -> GLFW.KeyState
    -> GLFW.ModifierKeys
    -> [(GLFW.Key, GLFW.ModifierKeys)]
    -> [(GLFW.Key, GLFW.ModifierKeys)]
  updateHeldKeys k GLFW.KeyState'Pressed  m ks = insert (k, m) ks
  updateHeldKeys k GLFW.KeyState'Released m ks = delete (k, m) ks
  updateHeldKeys _ _                      _ ks = ks

buttons :: Event t Input
  -> App t (Event t [GLFW.MouseButton], Dynamic t [GLFW.MouseButton])
buttons eInput = do
  let pressed = ffilter (not . null) . fmap filterPressed $ eInput
  held <- foldDyn (flip (foldl (flip $ uncurry3 updateHeldButtons)) . inputMouseButtons) [] eInput
  return (pressed, held)
 where
  filterPressed = fmap fst3
    . filter ((== GLFW.MouseButtonState'Pressed) . snd3)
    . inputMouseButtons

  updateHeldButtons :: GLFW.MouseButton
    -> GLFW.MouseButtonState
    -> GLFW.ModifierKeys
    -> [GLFW.MouseButton]
    -> [GLFW.MouseButton]
  updateHeldButtons b GLFW.MouseButtonState'Pressed  _ bs = insert b bs
  updateHeldButtons b GLFW.MouseButtonState'Released _ bs = delete b bs

pointer :: Event t CursorPosition -> Dynamic t Camera -> App t (Dynamic t (V3 Float))
pointer cursorE cameraD = do
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
    toNdc d a = 2 * (a - (d / 2)) / d

  screenPosition :: Int -> Int -> Camera -> V3 Float -> V2 Float
  screenPosition screenWidth screenHeight camera pos =
    let width = realToFrac screenWidth
        height = realToFrac screenHeight
        persM = perspectiveProjection $ width / height
        viewM = toViewMatrix camera
        V4 x y _ w = persM !*! viewM !* point pos
    in (V2 width height * (V2 (x / w) (-y / w) + 1)) / 2
