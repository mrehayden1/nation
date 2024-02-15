module Render.Debug (
  createDebugGizmoOverlayer,
  createDebugInfoOverlayer,
  createDebugQuadOverlayer
) where

import Control.Lens
import Control.Monad
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.StateVar
import Data.Time.Clock.POSIX
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import Linear ((!*!))
import qualified Linear as L
import Text.Printf

import App
import Camera (Camera(..))
import qualified Camera as Cam
import Render.Element
import qualified Render.Matrix as M
import Render.Model
import Render.Pipeline
import Render.Text
import Render.Util

data FpsStatistics = FpsStatistics {
  fpsMean :: DeltaT,
  fpsLow :: DeltaT,
  fpsHigh :: DeltaT
}

createDebugGizmoOverlayer :: Env -> IO (WorldState -> IO ())
createDebugGizmoOverlayer env = do
  pipeline <- compilePipeline [
      ("debug/gizmo", GL.FragmentShader),
      ("debug/gizmo", GL.VertexShader)
    ]
  let modelMUniform = pipelineUniform pipeline "modelM"
      viewMUniform = pipelineUniform pipeline "viewM"
      projectionMUniform = pipelineUniform pipeline "projectionM"
  -- Load gizmo
  model <- fromGlbFile "assets/models/reference_frame.glb"
  return $ \WorldState{..} -> do
    bindPipeline pipeline
    modelM <- M.toGlMatrix (L.identity :: L.M44 GL.GLfloat)
    modelMUniform $= modelM
    let cameraViewM = Cam.toViewMatrix camera :: L.M44 GL.GLfloat
    -- Make the view matrix translation fixed so the gizmo is always in view.
    viewM <- M.toGlMatrix . (M.translate 0 0 (-100) !*!)
               . set L._w (L.V4 0 0 0 1) . L.m33_to_m44
               $ cameraViewM ^. L._m33
    viewMUniform $= viewM
    projection <- M.toGlMatrix . M.perspectiveProjection 0.1 1000
      . windowAspectRatio $ env
    projectionMUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    GL.clear [GL.DepthBuffer]
    renderModel pipeline model
    unbindPipeline
    return ()

createDebugInfoOverlayer :: Env -> IORef POSIXTime -> IO (Frame -> IO ())
createDebugInfoOverlayer env timeRef = do
  font <- loadFont "bpdots.squares-bold"
  renderText <- createDebugTextRenderer . windowAspectRatio $ env
  deltasRef <- newIORef []
  fpsRef <- newIORef Nothing
  return . renderDebugText deltasRef fpsRef font $ renderText
 where
  renderDebugText :: IORef [DeltaT]
    -> IORef (Maybe (POSIXTime, FpsStatistics))
    -> MsdfFont
    -> (Text -> IO ())
    -> Frame
    -> IO ()
  renderDebugText deltasRef fpsRef font renderText (Input{..}, Output{..}) = do
    let WorldState{..} = worldState
    time' <- readIORef timeRef
    when (deltaT > 0) . modifyIORef deltasRef $ (deltaT :)
    deltas <- readIORef deltasRef
    modifyIORef fpsRef (maybe (Just (time', FpsStatistics 0 0 0)) Just)
    (lastUpdated, _) <- fmap fromJust . readIORef $ fpsRef
    -- Update the stored fps count every half second only if there are deltas
    when ((not . null $ deltas) && floor (time' * 2) > (floor (lastUpdated * 2) :: Int)) $ do
      let fpss = fmap recip deltas
          n = length fpss
          fpsSum = sum fpss
          fpss' = List.sort fpss
          mean = if fpsSum > 0
                   then fpsSum / realToFrac n
                   else 0
          stats = FpsStatistics {
                    fpsMean = mean,
                    fpsLow = head fpss',
                    fpsHigh = fpss' !! (n - 1)
                  }
      writeIORef fpsRef . Just $ (time', stats)
      writeIORef deltasRef []
    (_, fpsStats) <- fmap fromJust . readIORef $ fpsRef
    renderLines $ [
        -- FPS counter
        printf "Frame rate",
        printf "  mean  : %    dfps" (round . fpsMean $ fpsStats :: Int),
        printf "  low   : %    dfps" (round . fpsLow $ fpsStats :: Int),
        printf "  high  : %    dfps" (round . fpsHigh $ fpsStats :: Int),
        -- Player position
        uncurry (printf "Player: x% .5f y 0.00000 z% .5f") playerPosition
      ] ++ cameraInfo camera
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
        -- TODO render the text as one draw call
        t <- createDebugText font size (left, top n) l
        renderText t
        deleteText t

    cameraInfo :: (Floating a, PrintfArg a) => Camera a -> [String]
    cameraInfo Camera{..} =
      let L.V3 x y z = camPos
      in [
        printf "Camera: x% .5f y% .5f z% .5f" x y z,
        printf "        pitch% .1f yaw% .1f" (180 * camPitch / pi)
          . (/ pi) . (* 180) $ camYaw
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
  pipeline <- compilePipeline [
      ("debug/depth-map-debug-quad", GL.FragmentShader),
      ("debug/depth-map-debug-quad", GL.VertexShader)
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
    bindPipeline pipeline
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texture
    GL.bindVertexArrayObject $= Just vertexArrayObject
    GL.clear [GL.DepthBuffer]
    GL.drawArrays GL.TriangleStrip 0 . fromIntegral $ length vertices `div` 5
    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing
