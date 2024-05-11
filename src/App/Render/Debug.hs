module App.Render.Debug (
  createConsoleOverlayer,
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
import qualified Data.Vector as V
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import Linear
import Text.Printf

import App.Camera (Camera(..))
import qualified App.Camera as Cam
import App.Game hiding (Env)
import App.Matrix
import App.Projection
import App.Render.Env
import App.Render.Model
import App.Render.Pipeline
import App.Render.Text
import App.Render.Util
import App.Vector

data FrameTimes = FrameTimes {
  frameTimeMean :: Float,
  frameTimeLow :: Float,
  frameTimeHigh :: Float
}

-- TODO Refactor this and createDebugInfoOverlayer
--  - Pre-load font
--  - Refactor out screen position logic
createConsoleOverlayer :: IO (Frame -> Render ())
createConsoleOverlayer = do
  renderText <- createTextRenderer
  font <- loadFont "bpdots.squares-bold"
  return $ renderConsole renderText font
 where
  renderConsole :: (Text -> Bool -> Render ())
    -> MsdfFont
    -> Frame
    -> Render ()
  renderConsole renderText font (_, Output{..}) = do
    -- FIXME duplicated from renderLines
    aspectRatio <- asks viewportAspectRatio
    let bottom = if aspectRatio > 1
                   then negate . recip $ aspectRatio
                   else -1
        left = negate . min 1 $ aspectRatio
        size = 0.02
    liftIO $ GL.clear [GL.DepthBuffer]
    t <- createText font size (left, bottom) $ "> " ++ outputConsoleText
           ++ "_"
    renderText t True
    deleteText t

createDebugGizmoOverlayer :: IO (Camera Float -> Render ())
createDebugGizmoOverlayer = do
  pipeline <- compilePipeline [
      ("debug/gizmo", GL.FragmentShader),
      ("debug/gizmo", GL.VertexShader)
    ]
  -- Load gizmo
  model <- fromGlbFile "assets/models/reference_frame.glb"
  return $ \camera -> local (set _envPipeline pipeline) $ do
    liftIO . bindPipeline $ pipeline
    let cameraViewM = Cam.toViewMatrix camera :: M44 GL.GLfloat
    -- Make the view matrix translation fixed so the gizmo is always in view.
    viewM <- liftIO . toGlMatrix . (translate (V3 0 0 (-100)) !*!)
               . set translation 0 $ cameraViewM
    pipelineUniform pipeline "viewM" $= viewM
    aspectRatio <- asks viewportAspectRatio
    projection <- liftIO $ toGlMatrix . perspectiveProjection $ aspectRatio
    pipelineUniform pipeline "projectionM"
      $= (projection :: GL.GLmatrix GL.GLfloat)
    liftIO . GL.clear $ [GL.DepthBuffer]
    let globalTransforms = makeGlobalTransforms model Nothing
    V.zipWithM_ renderNode (modelNodes model) globalTransforms
    liftIO unbindPipeline
    return ()
 where
  renderNode :: Node -> M44 Float -> Render ()
  renderNode Node{..} bindMatrix = do
    mapM_ (mapM_ (renderMeshPrimitive bindMatrix)) nodeMesh

  renderMeshPrimitive :: M44 Float -> MeshPrimitive -> Render ()
  renderMeshPrimitive bindMatrix' meshPrim = do
    pipeline <- asks envPipeline
    let Material{..} = meshPrimMaterial meshPrim
    -- Set textures
    pipelineUniform pipeline "baseColorFactor"
      $= toGlVector4 materialBaseColorFactor
    --   Whether an base color texture is set
    pipelineUniform pipeline "hasBaseColorTexture"
      $= maybe (0 :: GL.GLint) (const 1) materialBaseColorTexture
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= materialBaseColorTexture
    -- Bind natrix
    bindMatrix <- liftIO . toGlMatrix $ bindMatrix'
    pipelineUniform pipeline "bindM" $= bindMatrix
    -- Draw
    GL.bindVertexArrayObject $= (Just . meshPrimVao $ meshPrim)
    liftIO $ GL.drawElements (meshPrimGlMode meshPrim)
                             (meshPrimNumIndices meshPrim)
                             GL.UnsignedInt
                             nullPtr
    GL.bindVertexArrayObject $= Nothing

createDebugInfoOverlayer :: IORef POSIXTime -> IO (Frame -> Render ())
createDebugInfoOverlayer timeRef = do
  font <- loadFont "bpdots.squares-bold"
  renderText <- createTextRenderer
  deltasRef <- newIORef []
  fpsRef <- newIORef Nothing
  return . renderDebugText deltasRef fpsRef font $ renderText
 where
  renderDebugText :: IORef [Float]
    -> IORef (Maybe (POSIXTime, FrameTimes))
    -> MsdfFont
    -> (Text -> Bool -> Render ())
    -> Frame
    -> Render ()
  renderDebugText deltasRef frameTimesRef font renderText (Input{..}, Output{..}) = do
    let World{..} = outputWorld
        Camera{..} = worldCamera
        V3 camX camY camZ = camPos
        (cursorX, cursorY) = inputCursorPos
        V3 pointerX pointerY pointerZ = worldPointerPosition
        V3 playerX playerY playerZ = worldPlayerPosition
        V3 playerVx playerVy playerVz = worldPlayerVelocity
    FrameTimes{..} <- fpsStats
    renderLines [
        -- FPS counter
        printf
          "Frame time: mean % .1fms , low % .1fms , high % .1fms"
          (frameTimeMean * 1000)
          (frameTimeLow * 1000)
          (frameTimeHigh * 1000),
        -- Player position
        printf "Player: x% .5f y x% .5f z% .5f" playerX playerY playerZ,
        printf "        x% .5f y x% .5f z% .5f m/s" playerVx playerVy playerVz,
        printf "Camera: x% .5f y% .5f z% .5f" camX camY camZ,
        printf "        pitch% .1f yaw% .1f" (180 * camPitch / pi)
          . (/ pi) . (* 180) $ camYaw,
        -- Keys
        printf "Held keys: %s" . List.intercalate ", "
          . fmap (drop 4 . show . fst) $ outputDebugKeys,
        -- Mouse buttons
        printf "Held buttons: %s" . List.intercalate ", "
          . fmap (drop 12 . show) $ outputDebugMouseButtons,
        -- Cursor stuff
        printf "Cursor screen %.5f %.5f" cursorX cursorY,
        printf "       world  %.5f %.5f %.5f" pointerX pointerY pointerZ
      ]
   where
    renderLines :: [String] -> Render ()
    renderLines ls = do
      -- Defined in "debug text space". See Text module.
      aspectRatio <- asks viewportAspectRatio
      let screenTop = if aspectRatio > 1
                        then recip aspectRatio
                        else 1
          screenLeft = negate . min 1 $ aspectRatio
          padding = 0.0125
          size = 0.02
          left = screenLeft + padding
          top line = screenTop - padding - line * size
      liftIO $ GL.clear [GL.DepthBuffer]
      forM_ (zip [1..] ls) $ \(n, l) -> do
        -- TODO render text as one draw call
        t <- createText font size (left, top n) l
        renderText t True
        deleteText t

    fpsStats :: Render FrameTimes
    fpsStats = do
      time' <- liftIO $ readIORef timeRef
      when (inputDeltaT > 0) . liftIO . modifyIORef deltasRef $ (inputDeltaT :)
      deltas <- liftIO $ readIORef deltasRef
      liftIO $
        modifyIORef frameTimesRef (maybe (Just (time', FrameTimes 0 0 0)) Just)
      (lastUpdated, _) <- fmap fromJust . liftIO . readIORef $ frameTimesRef
      -- Update the stored fps count every half second only if there are deltas
      when ((not . null $ deltas) && floor (time' * 2) > (floor (lastUpdated * 2) :: Int)) $ do
        let n = length deltas
            deltaSum = sum deltas
            deltasSorted = List.sort deltas
            deltaMean = if deltaSum > 0 then deltaSum / realToFrac n else 0
            stats = FrameTimes {
                      frameTimeMean = deltaMean,
                      frameTimeLow = head deltasSorted,
                      frameTimeHigh = deltasSorted !! (n - 1)
                    }
        liftIO . writeIORef frameTimesRef . Just $ (time', stats)
        liftIO $ writeIORef deltasRef []
      fmap (snd . fromJust) . liftIO . readIORef $ frameTimesRef

createDebugQuadOverlayer :: GL.TextureObject -> IO (Render ())
createDebugQuadOverlayer texture = do
  -- Create and bind vertex array object before our vertex and element buffers
  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject
  -- Create and bind vertex buffer object
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  -- Load vertices into array buffer
  liftIO $ withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (0.0 :: GL.GLfloat)
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Define vertex attribute pointers
  let positionAttributeLocation = GL.AttribLocation 0
      positionAttributeWidth = 3
      textureCoordinateAttributeLocation = GL.AttribLocation 1
      textureCoordinateAttributeWidth = 2
      sizeOfVertexUnit = fromIntegral . sizeOf $ (0.0 :: GL.GLfloat)
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
  pipeline <- liftIO $ compilePipeline [
      ("debug/depth-map-debug-quad", GL.FragmentShader),
      ("debug/depth-map-debug-quad", GL.VertexShader)
    ]
  return $ overlayDebugQuad pipeline vertexArrayObject
 where
  vertices :: [GL.GLfloat]
  vertices = [
    -- Position    Texture co-ords
    -- x   y   z   x   y
      -1,  1,  0,  0,  1,
      -1, -1,  0,  0,  0,
       1,  1,  0,  1,  1,
       1, -1,  0,  1,  0
    ]

  overlayDebugQuad :: Pipeline -> GL.VertexArrayObject -> Render ()
  overlayDebugQuad pipeline vertexArrayObject = do
    liftIO $ bindPipeline pipeline
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texture
    GL.bindVertexArrayObject $= Just vertexArrayObject
    liftIO $ GL.clear [GL.DepthBuffer]
    liftIO $ GL.drawArrays GL.TriangleStrip 0 . fromIntegral $ length vertices
               `div` 5
    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing
