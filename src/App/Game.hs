module App.Game (
  module App.Input,
  module App.Output,

  Env(..),
  MsaaSubsamples(..),

  Frame,

  app
) where

import Control.Applicative
import Control.Monad
import Data.Fixed
import Data.Functor
import Data.List (delete, insert)
import Data.Tuple.Extra
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Reflex
import Reflex.Network

import App.Camera
import qualified App.Camera as Cam
import App.Cursor
import App.Env
import App.Game.Coins
import App.Game.Peasant
import App.Input
import App.Projection
import App.Output

data MsaaSubsamples = MsaaNone | Msaa2x | Msaa4x | Msaa8x | Msaa16x
  deriving (Eq, Enum)

type Frame = (Input, Output)

buttonMousePrimary :: GLFW.MouseButton
buttonMousePrimary = GLFW.MouseButton'1

buttonMouseSecondary :: GLFW.MouseButton
buttonMouseSecondary = GLFW.MouseButton'2

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

playerStartPosition :: V3 Float
playerStartPosition = V3 0 0 0

playerStartDirection :: V3 Float
playerStartDirection = V3 1 0 0

epsilon :: Float
epsilon = 0.01

debugCameraSpeed :: Float
debugCameraSpeed = 6

app :: forall t. App t (Dynamic t Frame)
app = do
  inputE <- asks envInputE
  deltaT <- holdDyn 0 . fmap inputDeltaT $ inputE
  animationT <- foldDyn (+) 0 . fmap inputDeltaT $ inputE
  -- Keys
  (keyPresses, heldKeys) <- keys inputE
  -- Mouse buttons
  (buttonPresses, heldButtons) <- buttons inputE
  let lClickE = void . ffilter (elem buttonMousePrimary) $ buttonPresses
      rClickE = void . ffilter (elem buttonMouseSecondary) $ buttonPresses
  -- Cursor window position
  let cursorE = fmap inputCursorPos inputE
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
      . tag (current pointerD) $ lClickE
    playerVelocity' <- playerVelocity deltaT playerVelocityCurrent
                         moveSelection playerPosition
    let playerVelocityCurrent = current playerVelocity'
    playerDirection <- holdDyn playerStartDirection . ffilter (/= 0)
      . updated $ playerVelocity'
    playerPosition <- foldDyn (+) playerStartPosition
      . attachWith
          (^*)
          (current playerVelocity')
      . fmap inputDeltaT
      $ inputE
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
  (looseCoins, playerCoins) <- coins rClickE playerPosition playerDirection
  -- Peasants
  peasants' <- peasants looseCoins
  -- Output
  let worldState = World
        <$> animationT
        <*> camera
        <*> looseCoins
        <*> (Daylight <$> ambientLight <*> sunPitch <*> pure (pi / 8))
        <*> peasants'
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
  input <- holdDyn undefined inputE
  return $ (,) <$> input <*> output
 where
  playerVelocity :: Dynamic t Float
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
      in if norm delta <= epsilon
           then 0
           else normalize delta ^* speedMax

    speedMax = 4.5

  setPlayerCamera :: V3 Float -> Camera Float
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

  updateSunPitch :: Float -> [(GLFW.Key, GLFW.ModifierKeys)] -> Float -> Float
  updateSunPitch dt ks pitch = (+ pitch) . sum . fmap keyChange $ ks
   where
    keyChange (GLFW.Key'Equal, _) = angularVelocity * dt
    keyChange (GLFW.Key'Minus, _) = negate angularVelocity * dt
    keyChange _                   = 0
    -- Radians per second
    angularVelocity = 1

  debugCamera :: Dynamic t Float
    -> Dynamic t (Camera Float)
    -> Dynamic t Bool
    -> Dynamic t [(GLFW.Key, GLFW.ModifierKeys)]
    -> Dynamic t CursorPosition
    -> App t (Dynamic t (Camera Float))
  debugCamera delta playerCamera debugCameraOn heldKeys cursor = do
    -- Reset the debug camera to the last player camera position every time
    -- its toggled on.
    let camOn = attachPromptlyDyn playerCamera . updated $ debugCameraOn
    pos <- networkHold (return playerCamera)
             . fmap (uncurry debugCameraPosition) $ camOn
    return . join $ pos
   where
    debugCameraPosition :: Camera Float
      -> Bool
      -> App t (Dynamic t (Camera Float))
    debugCameraPosition camStart camOn = do
      s <- foldDyn (uncurry3 updateDebugCamera) ((0, 0), camStart)
       . gate (pure camOn)
       . updated
       $ (,,) <$> delta <*> fmap (fmap fst) heldKeys <*> cursor
      return $ fmap snd s

    updateDebugCamera :: Float
      -> [GLFW.Key]
      -> CursorPosition
      -> (CursorPosition, Camera Float)
      -> (CursorPosition, Camera Float)
    updateDebugCamera deltaT ks (x', y') ((x, y), camera@Camera{..}) =
      let velocity = pure debugCameraSpeed * pure deltaT
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

pointer :: Event t CursorPosition
  -> Behavior t (Camera Float)
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
    -> (Camera Float, V3 Float, V2 Float)
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
  worldPosition :: Int -> Int -> Camera Float -> V2 Float -> V3 Float
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

  screenPosition :: Int -> Int -> Camera Float -> V3 Float -> V2 Float
  screenPosition screenWidth screenHeight cam pos =
    let width = realToFrac screenWidth
        height = realToFrac screenHeight
        persM = perspectiveProjection $ width / height
        viewM = toViewMatrix cam
        V4 x y _ w = persM !*! viewM !* point pos
    in (V2 width height * (V2 (x / w) (-y / w) + 1)) / 2
