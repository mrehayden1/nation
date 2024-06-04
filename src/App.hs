module App (
  start
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock.POSIX
import qualified Graphics.UI.GLFW as GLFW
import Reflex
import Reflex.GLFW.Simple
import Reflex.Host.Headless
import Reflex.Network

import App.Collision.BVH as BVH
import App.Env
import App.Game
import App.Map
import App.Render
import App.Render.Model.GLTF as GLTF
import App.Render.Scene.Entity
import App.Window

appName :: String
appName = "Nation"

createRenderEnv :: IO App.Render.Env
createRenderEnv = do
  models <- loadModels
  jointModel <- GLTF.fromGlbFile "assets/models/joint.glb"
  return $ App.Render.Env {
    envJointModel = jointModel,
    envModels = models,
    envPipeline = undefined, --- FIXME yucky
    envRenderMeshes = True,
    envShowJoints = False,
    envViewportHeight = windowHeight,
    envViewportWidth = windowWidth
  }

start :: IO ()
start = do
  let seed = -662982938059047685
      MapData{..} = generateTestMapGeometry seed
  putStrLn $ "Starting " ++ appName ++ "..."
  -- Build the BVH of static map geometry
  let treesAndBounds = fmap (liftA2 (,) id aabb) mapTrees
      mapBVH = BVH.create' spatialBisection treesAndBounds
  bracket (initWindow appName) closeWindow $ \win -> do
    -- Used to get the time the frame was last refreshed
    timeRef <- newIORef 0
    -- Create renderer
    renderFrame <- createRenderer timeRef
    renderEnv <- createRenderEnv
    -- Enter game loop
    runHeadlessApp $ do
      startTime <- liftIO getPOSIXTime
      -- Write the start time to the time ref assuming that the post build
      -- event will happen immediately afterwards
      liftIO $ writeIORef timeRef startTime
      WindowReflexes{..} <- windowReflexes win
      -- Use the post build to create the first tick.
      ePostBuild <- getPostBuild
      (tickE, tickTrigger) <- newTriggerEvent
      -- Collect up the pressed keys during the current tick resetting them
      -- when the next tick comes
      let keys' = fmap (fmap reverse) . foldDyn (:) []
                    . fmap (\(k, _, s, m) -> (k, s, m))
      keysTick <- fmap join . networkHold (return $ pure [])
                    $ keys' key <$ tickE
      let buttons = foldDyn (:) [] . fmap (\(b, s, m) -> (b, s, m))
                      $ mouseButton
      buttonsTick <- fmap join . networkHold (return $ pure [])
                       $ buttons <$ tickE
      let inputE =
            attachPromptlyDynWith uncurry
              (Input <$> cursorPos <*> buttonsTick <*> keysTick)
              . leftmost $ [(0, 0) <$ ePostBuild, tickE]
      time <- holdDyn 0 . fmap inputTime $ inputE
      let appEnv = App.Env.Env {
            envInputE = inputE,
            envTime = time,
            envWindowHeight = windowHeight,
            envWindowWidth = windowWidth
          }
      frameE <- fmap updated . flip runReaderT appEnv $ game
      let shouldExitE = void . ffilter id . fmap (outputShouldExit . snd)
                          $ frameE
          shutdownE = leftmost [shouldExitE, windowClose]
      performEvent_
        . fmap (progressFrame startTime timeRef
                              (liftIO . tickTrigger)
                              (\frame -> liftIO $ do
                                 flip runRender renderEnv
                                   . renderFrame mapBVH $ frame
                                 swapBuffers win)
               )
        $ frameE
      return shutdownE
 where
  progressFrame :: MonadIO m
    => POSIXTime
    -> IORef POSIXTime
    -> ((Float, Float) -> m ()) -- Current time + deltaT
    -> (Frame -> m ())
    -> Frame
    -> m ()
  progressFrame startTime timeRef tickTrigger render frame = do
    render frame
    time' <- liftIO getPOSIXTime
    time <- liftIO . readIORef $ timeRef
    let delta = time' - time
    liftIO . writeIORef timeRef $ time'
    -- Progress the simulation one tick after we're finished rendering.
    tickTrigger (realToFrac (time' - startTime), realToFrac delta)
    -- Collect events to process in the next tick.
    liftIO GLFW.pollEvents
