module Render.Scene (
  createSceneRenderer
) where

import Control.Lens
import Data.StateVar
import Foreign
import Linear (M44)
import qualified Linear as L
import qualified Graphics.Rendering.OpenGL as GL

import App
import Camera as Cam
import Render.Model
import qualified Render.Matrix as M
import Render.Pipeline
import Vector as V

shadowMapTextureUnit :: Integral a => a
shadowMapTextureUnit = 0

albedoTextureUnit :: Integral a => a
albedoTextureUnit = 1

metallicRoughnessTextureUnit :: Integral a => a
metallicRoughnessTextureUnit = 2

normalMapTextureUnit :: Integral a => a
normalMapTextureUnit = 3

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
    bindPipeline pipeline
    -- Bind the shadow map texture
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Just shadowDepthMap
    -- Set view matrix
    let viewUniform = pipelineUniform pipeline "viewM"
    viewMatrix <- M.toGlMatrix .  Cam.toViewMatrix $ camera
    viewUniform $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set projection matrix
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projection <- M.toGlMatrix . M.perspectiveProjection 0.1 100
      . windowAspectRatio $ env
    projectionUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    let lightProjectionUniform = pipelineUniform pipeline "lightProjectionM"
    lightProjection <- M.toGlMatrix M.directionalLightProjection
    lightProjectionUniform $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    let lightViewUniform = pipelineUniform pipeline "lightViewM"
    lightView <- M.toGlMatrix . M.directionalLightViewMatrix (sunPitch sun)
                   . sunYaw $ sun
    lightViewUniform $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    let ambientIntensityUniform = pipelineUniform pipeline "ambientIntensity"
    ambientIntensityUniform $= daylightAmbientIntensity
    -- Set light direction
    let lightDirectionUniform = pipelineUniform pipeline "lightDirection"
    lightDirectionUniform $=
      (V.toGlVector3 . V.direction (sunPitch sun) . sunYaw $ sun)
    -- Set camera position
    let camPosUniform = pipelineUniform pipeline "camPos"
    camPosUniform $= (V.toGlVector3 . camPos $ camera)
    -- Render
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ (traverseModel_ (renderMeshPrimitive pipeline)) scene
    -- Unbind textures
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing

  renderMeshPrimitive :: Pipeline -> M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive pipeline modelMatrix MeshPrimitive{..} = do
    -- Set model matrix
    modelMatrix' <- M.toGlMatrix modelMatrix
    let modelMatrixUniform = pipelineUniform pipeline "modelM"
    modelMatrixUniform $= (modelMatrix' :: GL.GLmatrix GL.GLfloat)
    -- Set normal matrix
    let normalMatrixUniform = pipelineUniform pipeline "normalM"
    let transposeInverseModelMatrix = (^. L._m33) . L.transpose . L.inv44
          $ modelMatrix
    normalMatrix <- M.toGlMatrix . L.m33_to_m44 $ transposeInverseModelMatrix
    normalMatrixUniform $= (normalMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set albedo textures
    let albedoTexture = materialBaseColorTexture meshPrimMaterial
    GL.activeTexture $= GL.TextureUnit albedoTextureUnit
    GL.textureBinding GL.Texture2D $= Just albedoTexture
    -- Set metallic/roughness texture
    let metallicRoughnessTexture = materialMetallicRoughnessTexture
          meshPrimMaterial
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= Just metallicRoughnessTexture
    -- Set normal map
    let normalMap = materialNormalMap meshPrimMaterial
    GL.activeTexture $= GL.TextureUnit normalMapTextureUnit
    GL.textureBinding GL.Texture2D $= Just normalMap
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit albedoTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
