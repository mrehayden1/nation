module Main (
  main
) where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Reflex
import Reflex.GLFW.Simple
import Reflex.Host.Headless
import Reflex.Network

import App
import Entity
import Render
import Render.Model

appName :: String
appName = "Nation"

windowHeight, windowWidth :: Int
windowHeight = 1080
windowWidth = 1920

consoleDebuggingEnabled :: Bool
consoleDebuggingEnabled = False

fullscreen :: Bool
fullscreen = False

multisampleSubsamples :: MsaaSubsamples
multisampleSubsamples = Msaa16x

vsyncEnabled :: Bool
vsyncEnabled = False

createRenderEnv :: IO Render.Env
createRenderEnv = do
  jointModel <- fromGlbFile "joint" "assets/models/joint.glb"
  return $ Render.Env {
    envJointModel = jointModel,
    envPipeline = undefined, --- FIXME yucky
    envRenderMeshes = True,
    envShowJoints = False,
    envViewportHeight = windowHeight,
    envViewportWidth = windowWidth
  }

main :: IO ()
main = do
  putStrLn $ "Starting " ++ appName ++ "..."
  bracket initialiseGraphics shutdown $ \win -> do
    -- Used to get the time the frame was last refreshed
    timeRef <- newIORef 0
    -- Create graphics elements
    entities <- loadEntities
    -- Create renderer
    renderFrame <- createRenderer win timeRef entities
    renderEnv <- createRenderEnv
    -- Enter game loop
    let appEnv = App.Env {
      envEntities = entities,
      envWindowHeight = windowHeight,
      envWindowWidth = windowWidth
    }
    runHeadlessApp $ do
      time <- liftIO getPOSIXTime
      -- Write the start time to the time ref assuming that the post build
      -- event will happen immediately afterwards
      liftIO $ writeIORef timeRef time
      WindowReflexes{..} <- windowReflexes win
      -- Use the post build to create the first tick.
      ePostBuild <- getPostBuild
      (eTick, tickTrigger) <- newTriggerEvent
      -- Collect up the pressed keys during the current tick resetting them
      -- when the next tick comes
      let keys' = fmap (fmap reverse) . foldDyn (:) []
                    . fmap (\(k, _, s, m) -> (k, s, m))
      keysTick <- fmap join . networkHold (return $ pure [])
                    $ keys' key <$ eTick
      let buttons = foldDyn (:) [] . fmap (\(b, s, m) -> (b, s, m))
                      $ mouseButton
      buttonsTick <- fmap join . networkHold (return $ pure [])
                       $ buttons <$ eTick
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
                              (liftIO . flip runRender renderEnv . renderFrame))
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

initialiseGraphics :: IO GLFW.Window
initialiseGraphics = do
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
