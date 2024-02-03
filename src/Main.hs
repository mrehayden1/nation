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
import Data.Tuple.Extra
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import Reflex
import Reflex.GLFW.Simple
import Reflex.Host.Headless
import Reflex.Network
import Text.Printf

import App
import qualified Camera as Cam
import Element
import qualified Matrix as M
import Pipeline
import Text
import Util (bufferOffset)
import Vector

appName :: String
appName = "Nation"

appEnv :: Env
appEnv = Env {
  -- TODO Add debugging build flag?
  consoleDebuggingEnabled = True,
  debugInfoEnabledDefault = True,
  windowHeight = 1080,
  windowWidth = 1920,
--windowHeight = 768,
--windowWidth = 1024,
  vsyncEnabled = False
}

type AspectRatio = Float

windowAspectRatio :: Env -> AspectRatio
windowAspectRatio Env {..} = realToFrac windowWidth / realToFrac windowHeight

depthMapWidth, depthMapHeight, depthMapTextureImageLevel :: GL.GLsizei
depthMapWidth = 2048
depthMapHeight = 2048
depthMapTextureImageLevel = 0

-- Calculate the projection needed to build a depth map for directional light
directionalLightProjection :: (L.Epsilon a, Floating a) => L.M44 a
directionalLightProjection = L.ortho (-10) 10 (-10) 10 10 (-10)

{-
type DirectionalLight a = Floating a => L.V3 a
directionalLightProjection :: (L.Epsilon a, Floating a) => Camera a -> DirectionalLight a -> L.M44 a
directionalLightProjection camera lightDirection = 
  let cameraView = Cam.toViewMatrix camera
      cameraProjection = perspectiveProjection
      cameraInverseViewProjection = L.inv44 $ cameraProjection !*! cameraView
      frustum = fmap ((cameraInverseViewProjection !*) . L.point) ndcCube
      lightMatrix = directionalLightViewMatrix (negate lightDirection)
      lightSpaceFrustum = fmap (L.normalizePoint . (lightMatrix !*)) frustum
      top = maximum . fmap (^. L._y) $ lightSpaceFrustum
      bottom = minimum . fmap (^. L._y) $ lightSpaceFrustum
      left = minimum . fmap (^. L._x) $ lightSpaceFrustum
      right = maximum . fmap (^. L._x) $ lightSpaceFrustum
      near = maximum . fmap (^. L._z) $ lightSpaceFrustum
      far = minimum . fmap (^. L._z) $ lightSpaceFrustum
  in L.ortho left right bottom top near far
 where
  ndcCube :: Floating a => [L.V3 a]
  ndcCube = fmap (\(x, y, z) -> L.V3 x y z) [
    ( 1,  1, -1), ( 1,  1,  1), (-1,  1,  1), (-1,  1, -1),
    ( 1, -1, -1), ( 1, -1,  1), (-1, -1,  1), (-1, -1, -1)]
-}

-- Light direction points towards the (infinitely far away) light source
directionalLightViewMatrix :: (L.Epsilon a, Floating a) => L.V3 a -> L.M44 a
directionalLightViewMatrix direction = L.lookAt (negate direction) (L.V3 0 0 0) Cam.up

perspectiveProjection :: Floating a => a -> L.M44 a
perspectiveProjection aspectRatio = L.perspective (fov * pi / 180) aspectRatio near far
 where
  fov = 45
  near = 0.1
  far = 100

main :: IO ()
main = do
  putStrLn $ "Starting " ++ appName ++ "..."
  bracket initialise shutdown $ \win -> do
    -- Used to get the time the frame was last refreshed
    timeRef <- newIORef 0
    -- Create graphics elements
    sceneElements <- createSceneElements
    -- Create a depth buffer object and depth map texture
    (shadowDepthMapTexture, renderShadowDepthMap) <- createShadowDepthMapper sceneElements
    -- Create renderers
    renderScene <- createSceneRenderer appEnv sceneElements shadowDepthMapTexture
    overlayDebugQuad <- createDebugQuadOverlayer shadowDepthMapTexture
    debugTextOverlayer <- createDebugTextOverlayer appEnv timeRef
    let renderFrame frame@(_, Output{..}) = do
          renderShadowDepthMap worldState
          renderScene worldState
          when shouldOverlayLightDepthQuad overlayDebugQuad
          when shouldOverlayDebugInfo . debugTextOverlayer $ frame
          GLFW.swapBuffers win
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
      let keys = foldDyn (:) [] . fmap (\(k, _, s, m) -> (k, s, m))
      keys' <- networkHold (return $ pure []) $ keys key <$ eTick
      let eInput = attachPromptlyDynWith uncurry (Input <$> cursorPos <*> join keys')
                    . leftmost $ [(time, 0) <$ ePostBuild, eTick]
      eFrame <- fmap updated . flip runReaderT appEnv . game $ eInput
      let eShouldExit = void . ffilter id . fmap (shouldExit . snd) $ eFrame
          eShutdown = leftmost [eShouldExit, windowClose]
      performEvent_
        . fmap (progressFrame timeRef (liftIO . tickTrigger) (liftIO . renderFrame))
        $ eFrame
      return eShutdown
 where
  progressFrame :: MonadIO m
    => IORef POSIXTime
    -> ((Time, DeltaT) -> m ())
    -> ((Input, Output) -> m ())
    -> (Input, Output)
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

initialise :: IO GLFW.Window
initialise = do
  let Env {..} = appEnv
  r <- GLFW.init
  unless r (error "GLFW.init error.")
  GLFW.defaultWindowHints
  -- Stop window resizing
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  when consoleDebuggingEnabled $ do
    putStrLn "Console debugging enabled"
    GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
  _ <- GLFW.init
  window <- fmap (fromMaybe (error "GLFW failed to create window."))
    . GLFW.createWindow windowWidth windowHeight appName Nothing $ Nothing
  GLFW.makeContextCurrent (Just window)
  -- Enable console debugging output
  when consoleDebuggingEnabled $ do
    GL.debugOutput $= GL.Enabled
    GL.debugOutputSynchronous $= GL.Enabled
    GL.debugMessageCallback $= Just printDebugMessage
  -- MOUSE
  -- Hide cursor
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  -- Capture raw mouse motion if supported
  rawMouseSupported <- GLFW.rawMouseMotionSupported
  if rawMouseSupported
    then
      GLFW.setRawMouseMotion window True
    else
      putStrLn "Raw mouse motion unsupported."
  -- GRAPHICS
  -- Vsync
  GLFW.swapInterval $ if vsyncEnabled then 1 else 0
  -- Enable depth testing
  GL.depthFunc $= Just GL.Lequal
  -- Set clear colour
  GL.clearColor $= GL.Color4 0 0 0 1
  -- Enable blending
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
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

-- Create a texture object and a callback that renders the shadow depth map
-- from the perspective of out light source to the texture.
createShadowDepthMapper :: [RenderableElement] -> IO (GL.TextureObject, WorldState -> IO ())
createShadowDepthMapper sceneElements = do
  frameBuffer <- GL.genObjectName
  depthMap <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just depthMap
  GL.texImage2D GL.Texture2D GL.NoProxy depthMapTextureImageLevel GL.DepthComponent' (GL.TextureSize2D depthMapWidth depthMapHeight) 0 (GL.PixelData GL.DepthComponent GL.Float nullPtr)
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  -- Clamp to a max depth border so shadows don't appear when sampling
  -- outside of the depth map
  GL.textureBorderColor GL.Texture2D $= GL.Color4 1 1 1 1
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  -- Bind the depth map to the frame depth buffer
  GL.bindFramebuffer GL.Framebuffer $= frameBuffer
  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D depthMap depthMapTextureImageLevel
  GL.drawBuffer $= GL.NoBuffers -- Don't draw colour to our framebuffer
  GL.readBuffer $= GL.NoBuffers -- Don't read colour from our framebuffer
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind
  pipeline <- createPipeline [
      ("depth", GL.FragmentShader),
      ("depth", GL.VertexShader)
    ]
  return (depthMap, renderDepthMap frameBuffer pipeline)
 where
  renderDepthMap :: GL.FramebufferObject -> Pipeline -> WorldState -> IO ()
  renderDepthMap frameBuffer pipeline WorldState{..} = do
    GL.currentProgram $= (Just . pipelineProgram $ pipeline)
    GL.bindFramebuffer GL.Framebuffer $= frameBuffer
    -- Set projection matrix
    projection <- GL.newMatrix GL.RowMajor . M.unpack
      $ directionalLightProjection
    let projectionUniform = pipelineUniform pipeline "projectionM"
    GL.uniform projectionUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    viewMatrix <- GL.newMatrix GL.RowMajor . M.unpack
      . directionalLightViewMatrix $ daylightDirection
    let viewUniform = pipelineUniform pipeline "viewM"
    GL.uniform viewUniform $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set model matrix
    model <- GL.newMatrix GL.RowMajor . M.unpack $ L.identity
    let modelUniform = pipelineUniform pipeline "modelM"
    GL.uniform modelUniform $= (model :: GL.GLmatrix GL.GLfloat)
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size depthMapWidth depthMapHeight
      )
    GL.clear [GL.DepthBuffer]
    forM_ sceneElements renderElement
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind

createDebugTextOverlayer :: Env -> IORef POSIXTime -> IO (Frame -> IO ())
createDebugTextOverlayer env timeRef = do
  font <- loadFont "bpdots.squares-bold"
  renderText <- createDebugTextRenderer . windowAspectRatio $ env
  deltasRef <- newIORef []
  fpsRef <- newIORef Nothing
  return . renderDebugText deltasRef fpsRef font $ renderText
 where
  renderDebugText :: IORef [DeltaT]
    -> IORef (Maybe (POSIXTime, Float))
    -> MsdfFont
    -> (Text -> IO ())
    -> Frame
    -> IO ()
  renderDebugText deltasRef fpsRef font renderText (Input{..}, Output{..}) = do
    let WorldState{..} = worldState
    time' <- readIORef timeRef
    modifyIORef deltasRef (deltaT :)
    deltas <- readIORef deltasRef
    modifyIORef fpsRef (maybe (Just (time', 0)) Just)
    (lastUpdated, _) <- fmap fromJust . readIORef $ fpsRef
    -- Update the fps count every half second
    when (floor (time' * 2) > (floor (lastUpdated * 2) :: Int)) $ do
      let deltasS = sum deltas
          fps = if deltasS > 0
                  then ((/) . realToFrac . length $ deltas)
                         . realToFrac $ deltasS
                  else 0
      writeIORef fpsRef . Just $ (time', fps)
      writeIORef deltasRef []
    (_, fps) <- fmap fromJust . readIORef $ fpsRef
    -- TODO Text position dependent on aspect ratio
    fpsText <- createDebugText font 0.02 (-0.975, -0.54) . (++ " FPS") . show
      $ (round fps :: Int)
    renderText fpsText
    deleteText fpsText
    positionText <- createDebugText font 0.02 (-0.975, -0.52)
      . uncurry (printf "x:%s, y: 0.0, z:%s")
      . both (printf "% .6f" :: PosX -> String)
      $ playerPosition
    GL.clear [GL.DepthBuffer]
    renderText positionText
    deleteText positionText

createDebugQuadOverlayer :: GL.TextureObject -> IO (IO ())
createDebugQuadOverlayer texture = do
  -- Create and bind vertex array object before our vertex and element buffers
  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject
  -- Create and bind vertex buffer object
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  -- Load vertices into array buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (undefined :: VertexUnit)
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Define vertex attribute pointers
  let positionAttributeLocation = GL.AttribLocation 0
      positionAttributeWidth = 3
      textureCoordinateAttributeLocation = GL.AttribLocation 1
      textureCoordinateAttributeWidth = 2
      sizeOfVertexUnit = fromIntegral . sizeOf $ (0.0 :: VertexUnit)
      stride = 5
  --   Position
  GL.vertexAttribPointer positionAttributeLocation $=
    (GL.ToFloat, GL.VertexArrayDescriptor positionAttributeWidth GL.Float (sizeOfVertexUnit * stride) nullPtr)
  GL.vertexAttribArray positionAttributeLocation $= GL.Enabled
  --   Texture co-ordinates
  GL.vertexAttribPointer textureCoordinateAttributeLocation $=
    (GL.ToFloat, GL.VertexArrayDescriptor textureCoordinateAttributeWidth GL.Float (sizeOfVertexUnit * stride) . bufferOffset $ (sizeOfVertexUnit * positionAttributeWidth))
  GL.vertexAttribArray textureCoordinateAttributeLocation $= GL.Enabled
  -- Unbind the vertex array object and buffer object
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  pipeline <- createPipeline [
      ("depth-map-debug-quad", GL.FragmentShader),
      ("depth-map-debug-quad", GL.VertexShader)
    ]
  return $ overlayDebugQuad pipeline vertexArrayObject
 where
  vertices :: Vertices
  vertices = [
    -- Position    Texture co-ords
    -- x   y   z   x   y
      -1,  1,  0,  0,  1,
      -1, -1,  0,  0,  0,
       1,  1,  0,  1,  1,
       1, -1,  0,  1,  0
    ]

  overlayDebugQuad :: Pipeline -> GL.VertexArrayObject -> IO ()
  overlayDebugQuad pipeline vertexArrayObject = do
    GL.currentProgram $= (Just . pipelineProgram $ pipeline)
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texture
    GL.bindVertexArrayObject $= Just vertexArrayObject
    GL.clear [GL.DepthBuffer]
    GL.drawArrays GL.TriangleStrip 0 . fromIntegral $ length vertices `div` 5
    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing

createSceneRenderer :: Env -> [RenderableElement] -> GL.TextureObject -> IO (WorldState -> IO ())
createSceneRenderer env@Env{..} sceneElements shadowDepthMap = do
  pipeline <- createPipeline [
      ("shader", GL.FragmentShader),
      ("shader", GL.VertexShader)
    ]
  return $ render pipeline
 where
  render :: Pipeline -> WorldState -> IO ()
  render pipeline WorldState{..} = do
    GL.textureBinding GL.Texture2D $= Just shadowDepthMap
    GL.currentProgram $= (Just . pipelineProgram $ pipeline)
    -- Set projection matrix
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projection <- GL.newMatrix GL.RowMajor . M.unpack . perspectiveProjection
      . windowAspectRatio $ env
    GL.uniform projectionUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    let viewUniform = pipelineUniform pipeline "viewM"
    viewMatrix <- GL.newMatrix GL.RowMajor . M.unpack .  Cam.toViewMatrix $ camera
    -- View matrix for directional light
    --viewMatrix <- GL.newMatrix GL.RowMajor . M.unpack $ L.lookAt (negate daylightDirection) (L.V3 0 0 0) Cam.up
    GL.uniform viewUniform $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set model matrix
    let modelUniform = pipelineUniform pipeline "modelM"
    model <- GL.newMatrix GL.RowMajor . M.unpack $ L.identity
    GL.uniform modelUniform $= (model :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    let lightProjectionUniform = pipelineUniform pipeline "lightProjectionM"
    lightProjection <- GL.newMatrix GL.RowMajor . M.unpack $ directionalLightProjection
    GL.uniform lightProjectionUniform $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    let lightViewUniform = pipelineUniform pipeline "lightViewM"
    lightView <- GL.newMatrix GL.RowMajor . M.unpack $ directionalLightViewMatrix daylightDirection
    GL.uniform lightViewUniform $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    let ambientIntensityUniform = pipelineUniform pipeline "ambientIntensity"
    GL.uniform ambientIntensityUniform $= daylightAmbientIntensity
    -- Set light direction
    let lightDirectionUniform = pipelineUniform pipeline "lightDirection"
    GL.uniform lightDirectionUniform $= toGlVector3 daylightDirection
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    forM_ sceneElements renderElement
