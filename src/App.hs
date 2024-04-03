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

  game
) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Fixed
import Data.Functor
import Data.List (delete, insert, partition)
import Data.Time.Clock
import Data.Tuple.Extra
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Reflex
import Reflex.Network

import Camera
import qualified Camera as Cam
import Cursor
import Entity
import Entity.Collision
import Matrix
import Vector

data MsaaSubsamples = MsaaNone | Msaa2x | Msaa4x | Msaa8x | Msaa16x
  deriving (Eq, Enum)

data Env = Env {
  envEntities :: Entities,
  envWindowHeight :: Int,
  envWindowWidth :: Int
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
  daylightPitch :: Float,
  daylightYaw :: Float
}

data World = World {
  worldAnimationTime :: Float,
  worldCamera :: Camera,
  worldCoins :: [V3 Float],
  worldDaylight :: Daylight,
  worldPlayerCoins :: Int,
  worldPlayerDirection :: V3 Float,
  worldPlayerPosition :: V3 Float,
  worldPlayerVelocity :: V3 Float,
  worldPointerPosition :: V3 Float
}

playerStartPosition :: V3 Float
playerStartPosition = V3 0 0 0

playerStartDirection :: V3 Float
playerStartDirection = V3 0 0 0

epsilon :: Float
epsilon = 0.01

debugCameraSpeed :: Float
debugCameraSpeed = 6

type App t a = forall m. (Adjustable t m, MonadFix m, MonadHold t m,
  MonadReader Env m) => m a

game :: forall t. Event t Input -> App t (Dynamic t Frame)
game eInput = do
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
  overlayDebugInfo <- toggle False
    . ffilter (elem keyDebugInfoToggle . fmap fst) $ keyPresses
  debugCameraOn <- toggle False
    . ffilter (elem keyDebugCameraToggle . fmap fst)
    . gate (current . fmap not $ consoleOpen) $ keyPresses
  -- Player position
  rec
    moveSelection <- holdDyn (playerStartPosition + V3 epsilon 0 0)
      . tag (current pointerD) $ clicks
    playerVelocity' <- playerVelocity deltaT playerVelocityCurrent
                         moveSelection playerPosition
    let playerVelocityCurrent = current playerVelocity'
    playerDirection <- holdDyn playerStartDirection . ffilter (/= 0)
      . updated $ playerVelocity'
    playerPosition <- foldDyn (+) playerStartPosition
      . attachWith
          ((. realToFrac) . (^*))
          (current playerVelocity')
      . fmap inputDeltaT
      $ eInput
  -- Player camera
    let playerCamera = fmap setPlayerCamera pointerD
  -- Pointer
    pointerD <- pointer cursorE (current playerCamera)
      . current $ playerPosition
  -- Debug camera
  debugCam <- debugCamera deltaT playerCamera debugCameraOn heldKeys cursor
  let camera = debugCameraOn >>= \d -> if d then debugCam else playerCamera
  -- Lighting
  sunPitch <- foldDyn (uncurry updateSunPitch) (pi / 2) . updated
    $ (,) <$> deltaT <*> heldKeys
  let ambientLight = fmap ((* 1) . max 0 . sin) sunPitch
  -- Coins
  (coinsPositions, collectedCoinsE) <- coins playerPosition playerVelocity'
  playerCoins <- foldDyn (+) 0 collectedCoinsE
  -- Output
  let worldState = World
        <$> animationT
        <*> camera
        <*> coinsPositions
        <*> (Daylight <$> ambientLight <*> sunPitch <*> pure (pi / 8))
        <*> playerCoins
        <*> playerDirection
        <*> playerPosition
        <*> playerVelocity'
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
  playerVelocity :: Dynamic t DeltaT
    -> Behavior t (V3 Float)
    -> Dynamic t (V3 Float)
    -> Dynamic t (V3 Float)
    -> App t (Dynamic t (V3 Float))
  playerVelocity deltaT velocity destination origin =
    holdDyn 0
      . attachWith (\v (t, d, o) -> calculatePlayerVelocity v t d o) velocity
      . updated $ (,,) <$> deltaT <*> destination <*> origin
   where
    calculatePlayerVelocity v t dest orig =
      let delta = dest - orig
      in if magnitude delta <= epsilon
           then 0
           else normalize delta ^* speedMax

    speedMax = 4.5 -- m/s

  setPlayerCamera :: V3 Float -> Camera
  setPlayerCamera (V3 x _ z) =
    let th = (3 * pi) / 8
        h  = 16
    in Camera {
         camPitch = negate th,
         -- camera always looks at the player position...
         camPos = V3 x h (z + h / tan th),
         -- ...and towards -z
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

coins :: Dynamic t (V3 Float)
  -> Dynamic t (V3 Float)
  -> App t (Dynamic t [V3 Float], Event t Int)
coins playerPosition playerDirection = do
  Entities{..} <- asks envEntities
  coinPositionsAndCoinsCollected <-
    foldDyn (uncurry . collectCoins entitiesPlayer $ entitiesCoin)
            (initialCoins, 0)
      . updated . liftA2 (,) playerPosition $ playerDirection
  let live = fmap fst coinPositionsAndCoinsCollected
      collected = updated . fmap snd $ coinPositionsAndCoinsCollected
  return (live, collected)
 where
  initialCoins = [V3 5 0 5, V3 (-5) 0 5, V3 (-5) 0 (-5), V3 5 0 (-5)]

  collectCoins :: Player
    -> Coin
    -> V3 Float
    -> V3 Float
    -> ([V3 Float], Int)
    -> ([V3 Float], Int)
  collectCoins playerEntity coinEntity pos dir (cs, _) =
    let playerCollision' = transformCollision playerTransform
          . playerCollision $ playerEntity
        (V3 px _ pz) = pos
        (V3 dx _ dz) = dir
        rot = atan (dz/dx)
        playerTransform = translate2D (V2 px pz) !*! rotate2D rot
        (cs', collected) =
          partition (not . coinCollided playerCollision') cs
    in (cs', length collected)
   where
    coinCollided :: Collision -> V3 Float -> Bool
    coinCollided playerCollision' (V3 x _ z) =
      let coinCollision' = coinCollision coinEntity
      in collided playerCollision' . flip transformCollision coinCollision'
          . translate2D $ V2 x z

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

pointer :: Event t CursorPosition
  -> Behavior t Camera
  -> Behavior t (V3 Float)
  -> App t (Dynamic t (V3 Float))
pointer cursorE camera playerPos = do
  Env{..} <- ask
  cursorDelta <- fmap (fmap (uncurry $ flip (-)))
                   . foldDyn (flip $ (,) . snd) (0, 0)
                   . fmap (uncurry V2) $ cursorE
  foldDyn (updatePointer envWindowWidth envWindowHeight) 0
    . attachWith (uncurry (,,)) (liftA2 (,) camera playerPos) . updated $ cursorDelta
 where
  -- a pointer which remains within the bounds of a circle centered around the
  -- middle of the player
  updatePointer :: Int
    -> Int
    -> (Camera, V3 Float, V2 Float)
    -> V3 Float
    -> V3 Float
  updatePointer w h (cam, plPos@(V3 plX _ plZ), delta) curPos =
    -- project the new pointer position back into screen space and
    -- use that to calculate future updates
    let screenPos = screenPosition w h cam curPos
        screenPos' = screenPos + delta
        curPos'@(V3 x _ z) = worldPosition w h cam screenPos'
    in if (x - plX)**2 + (z - plZ)**2 <= r**2
         then curPos'
         else plPos + (normalize (curPos' - plPos) * pure r)
   where
    r = 7

  -- The game pointer world coordinates on the plane y = 0
  worldPosition :: Int -> Int -> Camera -> V2 Float -> V3 Float
  worldPosition screenWidth screenHeight cam (V2 cursorX cursorY) =
    let w = realToFrac screenWidth
        h = realToFrac screenHeight
        persM' = inversePerspectiveProjection $ w / h
        viewM' = inv44 . toViewMatrix $ cam
        xNdc =  toNdc w cursorX
        yNdc = -toNdc h cursorY
        V4 viewX viewY _ _ = persM' !* V4 xNdc yNdc (-1) 1
        V4 v1 v2 v3 _ = viewM' !* V4 viewX viewY (-1) 0
        V3 a b c = camPos cam
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
  screenPosition screenWidth screenHeight cam pos =
    let width = realToFrac screenWidth
        height = realToFrac screenHeight
        persM = perspectiveProjection $ width / height
        viewM = toViewMatrix cam
        V4 x y _ w = persM !*! viewM !* point pos
    in (V2 width height * (V2 (x / w) (-y / w) + 1)) / 2
