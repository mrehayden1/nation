module Main (
  main
) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.IORef
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
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
import System.Exit

import App
import Cursor
import qualified Camera as Cam
import Element
import qualified Matrix as M
import Pipeline
import Texture
import Util (bufferOffset)
import Vector

appName :: String
appName = "Nation"

-- TODO Debugging build flag
debugging :: Bool
debugging = True
--debugging = False

windowWidth, windowHeight :: Int
windowWidth = 1024
windowHeight = 1024

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

perspectiveProjection :: Floating a => L.M44 a
perspectiveProjection = L.perspective (fov * pi / 180) aspectRatio near far
 where
  fov = 45
  aspectRatio = 1
  near = 0.1
  far = 100

main :: IO ()
main = do
  putStrLn $ "Starting " ++ appName ++ "..."
  bracket initialise shutdown $ \win -> do
    timeRef <- newIORef Nothing
    -- Create graphics elements
    sceneElements <- createSceneElements
    -- Create a depth buffer object and depth map texture
    (shadowDepthMapTexture, renderShadowDepthMap) <- createShadowDepthMapper sceneElements
    overlayDebugQuad <- createDebugQuadOverlayer shadowDepthMapTexture
    renderScene <- createSceneRenderer sceneElements shadowDepthMapTexture
    let renderFrame Output{..} = do
          renderShadowDepthMap worldState
          when shouldOverlayLightDepthQuad overlayDebugQuad
          renderScene worldState
          GLFW.swapBuffers win
    -- Enter game loop
    runHeadlessApp $ do
      WindowReflexes {..} <- windowReflexes win
      -- Use the post build to create the first tick.
      ePostBuild <- getPostBuild
      (eTick, tickTrigger) <- newTriggerEvent
      -- Collect up the pressed keys during the current tick resetting them
      -- when the next tick comes
      let keys = foldDyn (:) [] . fmap (\(k, _, s, m) -> (k, s, m))
      keys' <- networkHold (return $ pure []) $ keys key <$ eTick
      let input = attachPromptlyDynWith uncurry (Input <$> cursorPos <*> join keys')
                    . leftmost $ [(0, 0) <$ ePostBuild, eTick]
      eOutput <- updated <$> app input
      let eShouldExit = void . ffilter id . fmap shouldExit $ eOutput
          eShutdown = leftmost [eShouldExit, windowClose]
      performEvent_
        . fmap (processFrame timeRef (liftIO . tickTrigger) (liftIO . renderFrame))
        $ eOutput
      return eShutdown
 where
  processFrame :: MonadIO m
    => IORef (Maybe POSIXTime)
    -> ((Time, DeltaT) -> m ())
    -> (Output -> m ())
    -> Output
    -> m ()
  processFrame timeRef tickTrigger render output = do
    render output
    time' <- liftIO getPOSIXTime
    time <- liftIO . fmap (fromMaybe time') . readIORef $ timeRef
    let delta = time' - time
    liftIO . writeIORef timeRef . Just $ time'
    -- Progress the simulation one tick after we're finished rendering.
    tickTrigger (time, delta)
    -- Collect events to process in the next tick.
    liftIO GLFW.pollEvents

initialise :: IO GLFW.Window
initialise = do
  r <- GLFW.init
  unless r (error "GLFW.init error.")
  GLFW.defaultWindowHints
  when debugging $ do
    putStrLn "Debugging enabled"
    GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
  _ <- GLFW.init
  window <- fmap (fromMaybe (error "GLFW failed to create window."))
    . GLFW.createWindow windowWidth windowHeight appName Nothing $ Nothing
  GLFW.makeContextCurrent (Just window)
  -- Enable console debugging output
  when debugging $ do
    GL.debugOutput $= GL.Enabled
    GL.debugOutputSynchronous $= GL.Enabled
    GL.debugMessageCallback $= Just printDebugMessage
  -- Hide cursor
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  -- Enable depth testing
  GL.depthFunc $= Just GL.Less
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
  print =<< get (GL.framebufferStatus GL.Framebuffer)
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
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.DepthBuffer]
    forM_ sceneElements renderElement
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind

createDebugQuadOverlayer :: GL.TextureObject -> IO (IO ())
createDebugQuadOverlayer depthMapTexture = do
  -- Create and bind vertex array object before our vertex and element buffers
  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject
  -- Create and bind vertex buffer object
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  -- Load vertices into array buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (0.0 :: VertexUnit)
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
  -- Unbind the vertex array object
  GL.bindVertexArrayObject $= Nothing
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
    GL.textureBinding GL.Texture2D $= Just depthMapTexture
    GL.bindVertexArrayObject $= Just vertexArrayObject
    GL.drawArrays GL.TriangleStrip 0 . fromIntegral . length $ vertices
    GL.bindVertexArrayObject $= Nothing

createSceneRenderer :: [RenderableElement] -> GL.TextureObject -> IO (WorldState -> IO ())
createSceneRenderer sceneElements shadowDepthMap = do
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
    projection <- GL.newMatrix GL.RowMajor . M.unpack $ perspectiveProjection
    -- Projection for directional light
    --projection <- GL.newMatrix GL.RowMajor . M.unpack $ directionalLightProjection
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
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    forM_ sceneElements renderElement
