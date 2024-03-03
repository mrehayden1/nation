module Render.Scene (
  createSceneRenderer
) where

import Control.Lens
import Data.Fixed
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
    viewMatrix <- M.toGlMatrix .  Cam.toViewMatrix $ camera
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set projection matrix
    projection <- M.toGlMatrix . M.perspectiveProjection 0.1 100
      . windowAspectRatio $ env
    pipelineUniform pipeline "projectionM" $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    lightProjection <- M.toGlMatrix M.directionalLightProjection
    pipelineUniform pipeline "lightProjectionM"
      $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    lightView <- M.toGlMatrix . M.directionalLightViewMatrix (sunPitch sun)
                   . sunYaw $ sun
    pipelineUniform pipeline "lightViewM"
      $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    pipelineUniform pipeline "ambientIntensity" $= daylightAmbientIntensity
    -- Set light direction
    pipelineUniform pipeline "lightDirection" $=
      (V.toGlVector3 . V.direction (sunPitch sun) . sunYaw $ sun)
    -- Set camera position
    pipelineUniform pipeline "camPos" $= (V.toGlVector3 . camPos $ camera)
    -- Render
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ (withRendererPosed (renderMeshPrimitive pipeline) (Just ("Walk", animationTime `mod'` 1))) scene
    --mapM_ (withRenderer (renderMeshPrimitive pipeline)) scene
    -- Unbind textures
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing

  renderMeshPrimitive :: Pipeline -> M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive pipeline modelMatrix MeshPrimitive{..} = do
    let Material{..} = meshPrimMaterial
    -- Textures
    -- Set albedo textures
    GL.activeTexture $= GL.TextureUnit albedoTextureUnit
    GL.textureBinding GL.Texture2D $= Just materialBaseColorTexture
    -- Set metallic/roughness texture
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= Just materialMetallicRoughnessTexture
    -- Set normal map
    GL.activeTexture $= GL.TextureUnit normalMapTextureUnit
    GL.textureBinding GL.Texture2D $= Just materialNormalMap
    -- Uniforms
    -- Alpha coverage
    pipelineUniform pipeline "alphaCutoff" $= materialAlphaCutoff
    pipelineUniform pipeline "alphaMode"
      $= (fromIntegral . fromEnum $ materialAlphaMode :: GL.GLint)
    -- Double-sidedness
    pipelineUniform pipeline "doubleSided"
      $= (fromIntegral . fromEnum $ materialDoubleSided :: GL.GLint)
    -- Set model matrix
    modelMatrix' <- M.toGlMatrix modelMatrix
    pipelineUniform pipeline "modelM"
      $= (modelMatrix' :: GL.GLmatrix GL.GLfloat)
    -- Set normal matrix
    normalMatrix <- M.toGlMatrix . L.m33_to_m44 . (^. L._m33) . L.transpose
      . L.inv44 $ modelMatrix
    pipelineUniform pipeline "normalM"
      $= (normalMatrix :: GL.GLmatrix GL.GLfloat)
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit albedoTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit normalMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
