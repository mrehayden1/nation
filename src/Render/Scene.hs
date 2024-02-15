module Render.Scene (
  createSceneRenderer
) where

import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import Linear as L

import App
import Camera as Cam
import Render.Model
import qualified Render.Matrix as M
import Render.Pipeline
import Vector as V

createSceneRenderer :: Env -> [Model] -> GL.TextureObject -> IO (WorldState -> IO ())
createSceneRenderer env@Env{..} scene shadowDepthMap = do
  pipeline <- compilePipeline [
      ("shader", GL.FragmentShader),
      ("shader", GL.VertexShader)
    ]
  return $ render pipeline
 where
  render :: Pipeline -> WorldState -> IO ()
  render pipeline WorldState{..} = do
    GL.textureBinding GL.Texture2D $= Just shadowDepthMap
    bindPipeline pipeline
    -- Set projection matrix
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projection <- M.toGlMatrix . M.perspectiveProjection 0.1 100
      . windowAspectRatio $ env
    projectionUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    let viewUniform = pipelineUniform pipeline "viewM"
    viewMatrix <- M.toGlMatrix .  Cam.toViewMatrix $ camera
    viewUniform $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set model matrix
    let modelUniform = pipelineUniform pipeline "modelM"
    model <- M.toGlMatrix (L.identity :: M44 GL.GLfloat)
    modelUniform $= (model :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    let lightProjectionUniform = pipelineUniform pipeline "lightProjectionM"
    lightProjection <- M.toGlMatrix M.directionalLightProjection
    lightProjectionUniform $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    let lightViewUniform = pipelineUniform pipeline "lightViewM"
    lightView <- M.toGlMatrix . M.directionalLightViewMatrix daylightDirection
                   $ Cam.worldUp
    lightViewUniform $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    let ambientIntensityUniform = pipelineUniform pipeline "ambientIntensity"
    ambientIntensityUniform $= daylightAmbientIntensity
    -- Set light direction
    let lightDirectionUniform = pipelineUniform pipeline "lightDirection"
    lightDirectionUniform $= V.toGlVector3 daylightDirection
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ (renderModel pipeline) scene
