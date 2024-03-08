module Render.Scene (
  module Render.Scene.Scene,

  createSceneRenderer
) where

import Control.Lens
import Data.StateVar
import Foreign
import Linear as L
import qualified Graphics.Rendering.OpenGL as GL

import Matrix
import Camera as Cam
import Render.Model
import Render.Pipeline
import Render.Env
import Render.Scene.Scene
import Vector

shadowMapTextureUnit :: Integral a => a
shadowMapTextureUnit = 0

baseColorTextureUnit :: Integral a => a
baseColorTextureUnit = 1

metallicRoughnessTextureUnit :: Integral a => a
metallicRoughnessTextureUnit = 2

normalTextureUnit :: Integral a => a
normalTextureUnit = 3

createSceneRenderer :: GL.TextureObject -> IO (RenderEnv -> Scene -> IO ())
createSceneRenderer shadowDepthMap = do
  pipeline <- compilePipeline [
      ("shader", GL.FragmentShader),
      ("shader", GL.VertexShader)
    ]
  return $ render pipeline
 where
  render :: Pipeline -> RenderEnv -> Scene -> IO ()
  render pipeline env@RenderEnv{..} Scene{..} = do
    let Daylight{..} = sceneDaylight
    bindPipeline pipeline
    -- Bind the shadow map texture
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Just shadowDepthMap
    -- Set view matrix
    viewMatrix <- toGlMatrix .  Cam.toViewMatrix $ sceneCamera
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set projection matrix
    projection <- toGlMatrix . perspectiveProjection . aspectRatio $ env
    pipelineUniform pipeline "projectionM" $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    lightProjection <- toGlMatrix directionalLightProjection
    pipelineUniform pipeline "lightProjectionM"
      $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    lightView <- toGlMatrix . directionalLightViewMatrix daylightPitch
                   $ daylightYaw
    pipelineUniform pipeline "lightViewM"
      $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set ambient intensity
    pipelineUniform pipeline "ambientIntensity" $= daylightAmbientIntensity
    -- Set light direction
    pipelineUniform pipeline "lightDirection" $=
      (toGlVector3 . eulerDirection daylightPitch $ daylightYaw)
    -- Set camera position
    pipelineUniform pipeline "camPos" $= (toGlVector3 . camPos $ sceneCamera)
    -- Render
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size (fromIntegral viewportWidth) (fromIntegral viewportHeight)
      )
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ (withRendererPosed (renderMeshPrimitive pipeline)
             <$> elementAnimation <*> elementPosition <*> elementModel)
          sceneElements
    -- Unbind textures
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing

  renderMeshPrimitive :: Pipeline -> M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive pipeline modelMatrix MeshPrimitive{..} = do
    let Material{..} = meshPrimMaterial
    -- Base color
    pipelineUniform pipeline "baseColorFactor"
      $= toGlVector4 materialBaseColorFactor
    pipelineUniform pipeline "hasBaseColorTexture"
      $= maybe (0 :: GL.GLint) (const 1) materialBaseColorTexture
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= materialBaseColorTexture
    -- Set metallic/roughness texture
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= materialMetallicRoughnessTexture
    -- Set normal map
    pipelineUniform pipeline "normalTextureScale" $= materialNormalTextureScale
    pipelineUniform pipeline "hasNormalTexture"
      $= maybe (0 :: GL.GLint) (const 1) materialNormalTexture
    GL.activeTexture $= GL.TextureUnit normalTextureUnit
    GL.textureBinding GL.Texture2D $= materialNormalTexture
    -- Alpha coverage
    pipelineUniform pipeline "alphaCutoff" $= materialAlphaCutoff
    pipelineUniform pipeline "alphaMode"
      $= (fromIntegral . fromEnum $ materialAlphaMode :: GL.GLint)
    -- Double-sidedness
    pipelineUniform pipeline "doubleSided"
      $= (fromIntegral . fromEnum $ materialDoubleSided :: GL.GLint)
    -- Set model matrix
    modelMatrix' <- toGlMatrix modelMatrix
    pipelineUniform pipeline "modelM"
      $= (modelMatrix' :: GL.GLmatrix GL.GLfloat)
    -- Set normal matrix
    normalMatrix <- toGlMatrix . m33_to_m44 . (^. _m33) . L.transpose
      . inv44 $ modelMatrix
    pipelineUniform pipeline "normalM"
      $= (normalMatrix :: GL.GLmatrix GL.GLfloat)
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit normalTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
