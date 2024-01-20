module Main (
  main
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
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
  containerTexture <- loadTexture
  mousePositionRef <- newIORef defaultCursorPosition
  keyEventsRef <- newIORef []
  win <- initWindow (cursorPositionHandler mousePositionRef) (keyHandler keyEventsRef)
  when debugging $ do
    GL.debugOutput $= GL.Enabled
    GL.debugOutputSynchronous $= GL.Enabled
    GL.debugMessageCallback $= Just debugCallback
  -- Enable depth testing
  GL.depthFunc $= Just GL.Less
  -- Create graphics pipeline
  pipeline <- createPipeline [
      ("shader", GL.FragmentShader),
      ("shader", GL.VertexShader)
    ]
  depthPipeline <- createPipeline [
      ("depth", GL.FragmentShader),
      ("depth", GL.VertexShader)
    ]
  depthMapDebugPipeline <- createPipeline [
      ("depth-map-debug-quad", GL.FragmentShader),
      ("depth-map-debug-quad", GL.VertexShader)
    ]
  -- Create a depth buffer object and depth map texture
  (depthMap, depthFrameBuffer) <- createDepthMap
  (debugQuadVertexArrayObject, debugQuadVertices) <- createDebugQuad
  -- Create scene
  sceneElements <- createSceneElements
  -- Create event loop
  _ <- runSpiderHost $ do
    (e, eTriggerRef) <- newEventWithTriggerRef
    -- Evaluate the app to set up the data flow graph
    behaviour <- runHostFrame $ app e
    -- Get the time the simulation starts
    startTime <- liftIO getPOSIXTime
    timeRef <- liftIO $ newIORef 0
    -- Create the event loop and run the Reflex app
    -- Begin the event processing loop until we get a quit signal
    iterateWhile id $ do
      time <- liftIO . readIORef $ timeRef
      time' <- liftIO $ fmap (subtract startTime) getPOSIXTime
      liftIO . writeIORef timeRef $ time'
      _ <- liftIO GLFW.pollEvents
      mETrigger <- liftIO $ readIORef eTriggerRef
      case mETrigger of
        Nothing ->
          -- Nobody is subscribed to the input Event, discard input
          return ()
        Just eTrigger -> do
          mousePosition <- liftIO $ readIORef mousePositionRef
          keyEvents <- liftIO $ readIORef keyEventsRef
          -- Clear the key buffer after we've read it since the callback is
          -- called for every key pressed
          liftIO $ writeIORef keyEventsRef []
          -- NB: Behavior is undefined if the same trigger is fired more than
          -- once per frame
          let input = Input mousePosition keyEvents time' $ time' - time
          fireEvents [eTrigger :=> Identity input]
      -- Retrieve the current output of the program and display it
      (worldState, shouldExit, overlayQuad) <- runHostFrame $ sample behaviour
      liftIO $ do
        --putStrLn "Frame"
        renderDepthMap depthFrameBuffer depthPipeline worldState sceneElements
        renderScene pipeline sceneElements depthMap worldState
        when overlayQuad
          . overlayDepthMapDebugQuad depthMapDebugPipeline containerTexture debugQuadVertexArrayObject
          $ debugQuadVertices
        GLFW.swapBuffers win
      return $ not shouldExit
  -- Clean up and terminate on exiting loop
  shutdown win
 where
  shutdown win = do
    putStrLn "Shutting down"
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

  initWindow :: GLFW.CursorPosCallback -> GLFW.KeyCallback -> IO GLFW.Window
  initWindow cursorPositionCallback keyCallback = do
    GLFW.defaultWindowHints
    -- TODO Disable debugging in production
    when debugging $ do
      putStrLn "Debugging enabled"
      GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
    _ <- GLFW.init
    window <- fmap (fromMaybe (error "GLFW failed to create window."))
             . GLFW.createWindow windowWidth windowHeight appName Nothing $ Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowCloseCallback window (Just shutdown)
    GLFW.setCursorPos window defaultCursorPositionX defaultCursorPositionY
    GLFW.setCursorPosCallback window (Just cursorPositionCallback)
    GLFW.setKeyCallback window (Just keyCallback)
    GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
    return window

  debugCallback (GL.DebugMessage _ _ _ _ msg) = putStrLn msg

  cursorPositionHandler :: IORef (GLFWMouseX, GLFWMouseY) -> GLFW.Window -> GLFWMouseX -> GLFWMouseY -> IO ()
  cursorPositionHandler mousePositionRef _ mouseX mouseY =
    writeIORef mousePositionRef (mouseX, mouseY)

  keyHandler :: IORef [(GLFW.Key, GLFW.KeyState)]
                  -> GLFW.Window
                  -> GLFW.Key
                  -> Int
                  -> GLFW.KeyState
                  -> GLFW.ModifierKeys
                  -> IO () 
  keyHandler keysRef _ key _ state _ =
    modifyIORef keysRef ((key, state) :)
    
  renderScene :: Pipeline -> [RenderableElement] -> GL.TextureObject -> WorldState -> IO ()
  renderScene pipeline sceneElements depthMap WorldState{..} = do
    GL.textureBinding GL.Texture2D $= Just depthMap
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
      
  createDepthMap :: IO (GL.TextureObject, GL.FramebufferObject)
  createDepthMap = do
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
    return (depthMap, frameBuffer)

  renderDepthMap :: GL.FramebufferObject -> Pipeline -> WorldState -> [RenderableElement] -> IO ()
  renderDepthMap frameBuffer pipeline WorldState{..} sceneElements = do
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

  createDebugQuad :: IO (GL.VertexArrayObject, Int)
  createDebugQuad = do
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
    return (vertexArrayObject, length vertices `div` fromIntegral stride)
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

  overlayDepthMapDebugQuad :: Pipeline -> GL.TextureObject -> GL.VertexArrayObject -> Int -> IO ()
  overlayDepthMapDebugQuad pipeline depthMap vertexArrayObject vertices = do
    GL.currentProgram $= (Just . pipelineProgram $ pipeline)
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just depthMap
    GL.bindVertexArrayObject $= Just vertexArrayObject
    GL.drawArrays GL.TriangleStrip 0 . fromIntegral $ vertices
    GL.bindVertexArrayObject $= Nothing
