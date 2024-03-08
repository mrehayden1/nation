module Render.Scene.Shadow (
  createShadowDepthMapper
) where

import Data.StateVar
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import Matrix
import Render.Model
import Render.Pipeline
import Render.Scene.Scene
import Vector

depthMapWidth, depthMapHeight, depthMapTextureImageLevel :: GL.GLsizei
depthMapWidth = 2048
depthMapHeight = 2048
depthMapTextureImageLevel = 0

baseColorTextureUnit :: Integral a => a
baseColorTextureUnit = 0

-- Create a texture object and a callback that renders the shadow depth map
-- from the perspective of out light source to the texture.
createShadowDepthMapper ::IO (GL.TextureObject, Scene -> IO ())
createShadowDepthMapper = do
  depthMap <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just depthMap
  GL.texImage2D
    GL.Texture2D
    GL.NoProxy
    depthMapTextureImageLevel
    GL.DepthComponent'
    (GL.TextureSize2D depthMapWidth depthMapHeight)
    0
    (GL.PixelData GL.DepthComponent GL.Float nullPtr)
  -- Disable texture filtering on our depth map
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  -- Clamp to a max depth border so shadows don't appear when sampling
  -- outside of the depth map
  GL.textureBorderColor GL.Texture2D $= GL.Color4 1 1 1 1
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  -- Unbind the texture
  GL.textureBinding GL.Texture2D $= Nothing
  -- Bind the depth map to the depth buffer FBO
  frameBuffer <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer $= frameBuffer
  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D depthMap depthMapTextureImageLevel
  GL.drawBuffer $= GL.NoBuffers -- Don't draw colour to our framebuffer
  GL.readBuffer $= GL.NoBuffers -- Don't read colour from our framebuffer
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind
  pipeline <- compilePipeline [
      ("depth", GL.FragmentShader),
      ("depth", GL.VertexShader)
    ]
  return (depthMap, renderDepthMap frameBuffer pipeline)
 where
  renderDepthMap :: GL.FramebufferObject -> Pipeline -> Scene -> IO ()
  renderDepthMap frameBuffer pipeline Scene{..} = do
    let Daylight{..} = sceneDaylight
    bindPipeline pipeline
    GL.bindFramebuffer GL.Framebuffer $= frameBuffer
    -- Set projection matrix
    projection <- toGlMatrix directionalLightProjection
    pipelineUniform pipeline "projectionM"
      $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    viewMatrix <- toGlMatrix . directionalLightViewMatrix daylightPitch
      $ daylightYaw
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set model matrix
    model <- toGlMatrix (identity :: M44 GL.GLfloat)
    pipelineUniform pipeline "modelM" $= (model :: GL.GLmatrix GL.GLfloat)
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size depthMapWidth depthMapHeight
      )
    GL.clear [GL.DepthBuffer]
    mapM_ (withRendererPosed (renderMeshPrimitive pipeline)
            <$> elementAnimation <*> elementPosition <*> elementModel)
          sceneElements
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind

  renderMeshPrimitive :: Pipeline -> M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive pipeline modelMatrix' MeshPrimitive{..} = do
    let Material{..} = meshPrimMaterial
    -- Set model matrix
    modelMatrix <- toGlMatrix modelMatrix'
    pipelineUniform pipeline "modelM"
      $= (modelMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set base color texture (for alpha testing)
    pipelineUniform pipeline "baseColorFactor"
      $= toGlVector4 materialBaseColorFactor
    --   Whether an base color texture is set
    pipelineUniform pipeline "hasBaseColorTexture"
      $= maybe (0 :: GL.GLint) (const 1) materialBaseColorTexture
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= materialBaseColorTexture
    -- Alpha
    pipelineUniform pipeline "alphaCutoff" $= materialAlphaCutoff
    pipelineUniform pipeline "alphaMode"
      $= (fromIntegral . fromEnum $ materialAlphaMode :: GL.GLint)
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    GL.bindVertexArrayObject $= Nothing
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
