module Render.Debug (
  createDebugTextOverlayer,
  createDebugQuadOverlayer
) where

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Linear as L
import Text.Printf

import App
import Camera as Cam
import qualified Graphics.Rendering.OpenGL as GL
import Render.Element
import Render.Pipeline
import Render.Text
import Render.Util

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
    -- Update the stored fps count every half second
    when (floor (time' * 2) > (floor (lastUpdated * 2) :: Int)) $ do
      let deltasS = sum deltas
          fps = if deltasS > 0
                  then ((/) . realToFrac . length $ deltas)
                         . realToFrac $ deltasS
                  else 0
      writeIORef fpsRef . Just $ (time', fps)
      writeIORef deltasRef []
    (_, fps) <- fmap fromJust . readIORef $ fpsRef
    renderLines $ [
        -- FPS counter
        printf "Frame rate: %dfps" (round fps :: Int),
        -- Player position
        uncurry (printf "Player: x% .5f y 0.00000 z% .5f") playerPosition
      ] ++ cameraLines camera
   where
    renderLines :: [String] -> IO ()
    renderLines ls = do
      -- Defined in "debug text space". See Text module.
      let aspectRatio = windowAspectRatio env
          screenTop = if aspectRatio > 1 then recip aspectRatio else 1
          screenLeft = negate $ min aspectRatio 1
          padding = 0.025
          size = 0.02
          left = screenLeft + padding
          top line = screenTop - padding - line * size
      GL.clear [GL.DepthBuffer]
      forM_ (zip [1..] ls) $ \(n, l) -> do
        t <- createDebugText font size (left, top n) l
        renderText t
        deleteText t

    cameraLines :: PrintfArg a => Camera a -> [String]
    cameraLines camera =
      let L.V3 x y z = Cam.position camera
      in [
        printf "Camera: x% .5f y% .5f z% .5f" x y z,
        printf "        pitch% .5f yaw% .5f" (Cam.pitch camera)
          . Cam.yaw $ camera
      ]

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


