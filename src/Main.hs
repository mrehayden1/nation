module Main (
  main
) where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Map
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Reflex
import Reflex.GLFW.Simple
import Reflex.Host.Headless
import Reflex.Network

import App
import Model
import Render.Debug
import Render.Env
import Render.Model
import qualified Render.Scene as Scene
import qualified Render.Scene.Shadow as Shadow

appName :: String
appName = "Nation"

windowHeight', windowWidth' :: Int
windowHeight' = 1080
windowWidth' = 1920

appEnv :: Env
appEnv = Env {
  -- TODO Add debugging build flag?
  consoleDebuggingEnabled = False,
  debugInfoEnabledDefault = True,
  fullscreen = False,
  multisampleSubsamples = Msaa16x,
  vsyncEnabled = False,
  windowHeight = windowHeight',
  windowWidth = windowWidth'
}

renderEnv :: RenderEnv
renderEnv = RenderEnv {
  viewportHeight = windowHeight',
  viewportWidth = windowWidth'
}

main :: IO ()
main = do
  putStrLn $ "Starting " ++ appName ++ "..."
  bracket initialiseGraphics shutdown $ \win -> do
    -- Used to get the time the frame was last refreshed
    timeRef <- newIORef 0
    -- Create graphics elements
    models <- loadModels
    -- Create renderer
    renderFrame <- createRenderer win timeRef models
    -- Enter game loop
    runHeadlessApp $ do
      time <- liftIO getPOSIXTime
      -- Write the start time to the time ref assuming that the post build
      -- event will happen immediately afterwards
      liftIO $ writeIORef timeRef time
      WindowReflexes {..} <- windowReflexes win
      -- Use the post build to create the first tick.
      ePostBuild <- getPostBuild
      (eTick, tickTrigger) <- newTriggerEvent
      -- Collect up the pressed keys during the current tick resetting them
      -- when the next tick comes
      let keys' = fmap (fmap reverse) . foldDyn (:) []
                    . fmap (\(k, _, s, m) -> (k, s, m))
      keysTick <- fmap join . networkHold (return $ pure [])
                    $ keys' key <$ eTick
      let buttons = foldDyn (:) []
                      . fmap (\(b, s, m) -> (b, s, m)) $ mouseButton
      buttonsTick <- fmap join
                       . networkHold (return $ pure []) $ buttons <$ eTick
      let eInput =
            attachPromptlyDynWith uncurry
              (Input <$> cursorPos <*> buttonsTick <*> keysTick)
              . leftmost $ [(time, 0) <$ ePostBuild, eTick]
      eFrame <- fmap updated . flip runReaderT appEnv . game $ eInput
      let eShouldExit = void . ffilter id . fmap (outputShouldExit . snd)
                          $ eFrame
          eShutdown = leftmost [eShouldExit, windowClose]
      performEvent_
        . fmap (progressFrame timeRef
                              (liftIO . tickTrigger)
                              (liftIO . renderFrame))
        $ eFrame
      return eShutdown
 where
  progressFrame :: MonadIO m
    => IORef POSIXTime
    -> ((Time, DeltaT) -> m ())
    -> (Frame -> m ())
    -> Frame
    -> m ()
  progressFrame timeRef tickTrigger render frame = do
    render frame
    time' <- liftIO getPOSIXTime
    time <- liftIO . readIORef $ timeRef
    let delta = time' - time
    liftIO . writeIORef timeRef $ time'
    -- Progress the simulation one tick after we're finished rendering.
    tickTrigger (time, delta)
    -- Collect events to process in the next tick.
    liftIO GLFW.pollEvents

createRenderer :: GLFW.Window
                    -> IORef POSIXTime
                    -> Map ModelName Model
                    -> IO (Frame -> IO ())
createRenderer win timeRef models = do
  -- Create a depth buffer object and depth map texture
  (shadowDepthMapTexture, renderShadowDepthMap)
    <- Shadow.createShadowDepthMapper
  renderScene <- Scene.createSceneRenderer shadowDepthMapTexture
  overlayConsole <- createConsoleOverlayer
  overlayDebugQuad <- createDebugQuadOverlayer shadowDepthMapTexture
  overlayDebugInfo <- createDebugInfoOverlayer timeRef
  overlayGizmo <- createDebugGizmoOverlayer
  -- TODO Move this to it's own definition
  -- React to changes in our Reflex application
  return $ \frame@(_, Output{..}) -> do
    -- TODO Move scene creation to its own definition
    let World{..} = outputWorld
        Daylight{..} = worldDaylight
        scene = Scene.Scene {
          sceneCamera = worldCamera,
          sceneElements = [
            {-
            Scene.Element {
              elementAnimation = Nothing,
              elementModel = models ! Monument,
              elementPosition = V3 3 0 3,
              elementRotation = Quaternion 1 0
            },
            Scene.Element {
              elementAnimation = Nothing,
              elementModel = models ! Fauna,
              elementPosition = V3 10 0 10,
              elementRotation = Quaternion 1 0
            }
            -}
            Scene.Element {
              elementAnimation = Nothing,
              elementModel = models ! Grass,
              elementPosition = 0,
              elementRotation = Quaternion 1 0
            },
            Scene.Element {
              elementAnimation = Just ("spin", 5, worldAnimationTime),
              elementModel = models ! Pointer,
              elementPosition = worldPointerPosition,
              elementRotation = Quaternion 1 0
            },
            Scene.Element {
              elementAnimation = Nothing,
              elementModel = models ! Horse,
              elementPosition = worldPlayerPosition,
              elementRotation = worldPlayerDirection
            }
          ],
          sceneDaylight = Scene.Daylight {
            daylightAmbientIntensity = daylightAmbientIntensity,
            daylightPitch = daylightSunPitch,
            daylightYaw = daylightSunYaw
          }
        }
    renderShadowDepthMap scene
    renderScene renderEnv scene
    when outputDebugQuadOverlay overlayDebugQuad
    when outputDebugInfoOverlay $ do
      overlayDebugInfo renderEnv frame
      unless outputDebugQuadOverlay . overlayGizmo renderEnv
        $ worldCamera
    when outputConsoleOpen $ overlayConsole renderEnv frame
    GLFW.swapBuffers win

initialiseGraphics :: IO GLFW.Window
initialiseGraphics = do
  let Env{..} = appEnv
  r <- GLFW.init
  unless r (error "GLFW.init error.")
  GLFW.defaultWindowHints
  -- Stop window resizing
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  when consoleDebuggingEnabled $ do
    putStrLn "Console debugging enabled"
    GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
  _ <- GLFW.init
  -- MSAA
  unless (multisampleSubsamples == MsaaNone) $
    GLFW.windowHint . GLFW.WindowHint'Samples . Just . round
      . ((2 :: Float) ^^) . fromEnum $ multisampleSubsamples
  -- Create window
  monitor' <- if fullscreen then GLFW.getPrimaryMonitor else return Nothing
  window <- fmap (fromMaybe (error "GLFW failed to create window."))
    . GLFW.createWindow windowWidth windowHeight appName monitor'
    $ Nothing
  GLFW.makeContextCurrent (Just window)
  -- Enable console debugging output
  when consoleDebuggingEnabled $ do
    GL.debugOutput $= GL.Enabled
    GL.debugOutputSynchronous $= GL.Enabled
    GL.debugMessageCallback $= Just printDebugMessage
  -- Hide cursor
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  -- Capture raw mouse motion if supported
  rawMouseSupported <- GLFW.rawMouseMotionSupported
  if rawMouseSupported
    then
      GLFW.setRawMouseMotion window True
    else
      putStrLn "Raw mouse motion unsupported."
  -- Vsync
  GLFW.swapInterval $ if vsyncEnabled then 1 else 0
  -- Enable depth testing
  GL.depthFunc $= Just GL.Lequal
  -- Set clear colour
  GL.clearColor $= GL.Color4 0 0 0 1
  -- Enable blending
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- Multisampling
  unless (multisampleSubsamples == MsaaNone) $ GL.multisample $= GL.Enabled
  return window
 where
  printDebugMessage :: GL.DebugMessage -> IO ()
  printDebugMessage (GL.DebugMessage _ _ _ _ msg) = putStrLn msg

shutdown :: GLFW.Window -> IO ()
shutdown w = do
  putStrLn "Shutting down..."
  GLFW.setWindowShouldClose w True
  GLFW.destroyWindow w
  GLFW.terminate
