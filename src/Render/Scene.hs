module Render.Scene (
  createSceneRenderer
) where

import Control.Monad
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import Linear as L

import App
import Camera as Cam
import Render.Element
import Render.Pipeline
import Matrix as M
import Vector as V

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
    lightView <- GL.newMatrix GL.RowMajor . M.unpack . directionalLightViewMatrix daylightDirection $ Cam.up
    GL.uniform lightViewUniform $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    let ambientIntensityUniform = pipelineUniform pipeline "ambientIntensity"
    GL.uniform ambientIntensityUniform $= daylightAmbientIntensity
    -- Set light direction
    let lightDirectionUniform = pipelineUniform pipeline "lightDirection"
    GL.uniform lightDirectionUniform $= V.toGlVector3 daylightDirection
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    forM_ sceneElements renderElement
