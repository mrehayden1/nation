module App.Render.Scene.Shadow (
  createShadowMapper
) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.StateVar
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import App.Matrix
import App.Render.Env
import App.Render.Model
import App.Render.Pipeline
import App.Render.Scene.Scene
import App.Vector

shadowMapWidth, shadowMapHeight, shadowMapTextureImageLevel :: GL.GLsizei
shadowMapWidth = 2048
shadowMapHeight = 2048
shadowMapTextureImageLevel = 0

baseColorTextureUnit :: Integral a => a
baseColorTextureUnit = 0

-- Create a texture object and a callback that renders the depth of the scene
-- from the perspective of our light source to that texture.
createShadowMapper ::IO (GL.TextureObject, Scene -> Render ())
createShadowMapper = do
  shadowMap <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just shadowMap
  GL.texImage2D
    GL.Texture2D
    GL.NoProxy
    shadowMapTextureImageLevel
    GL.DepthComponent'
    (GL.TextureSize2D shadowMapWidth shadowMapHeight)
    0
    (GL.PixelData GL.DepthComponent GL.Float nullPtr)
  -- Disable texture filtering on our depth map
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  -- Clamp sampling to a border equal to max depth so that shadows don't appear
  -- when sampling outside of the depth map
  GL.textureBorderColor GL.Texture2D $= GL.Color4 1 1 1 1
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  -- Unbind the texture
  GL.textureBinding GL.Texture2D $= Nothing
  -- Bind the depth map to the depth buffer FBO
  frameBuffer <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer $= frameBuffer
  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D shadowMap shadowMapTextureImageLevel
  GL.drawBuffer $= GL.NoBuffers -- Don't draw colour to our framebuffer
  GL.readBuffer $= GL.NoBuffers -- Don't read colour from our framebuffer
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind
  pipeline <- compilePipeline [
      ("depth", GL.FragmentShader),
      ("depth", GL.VertexShader)
    ]
  return (
     shadowMap,
     local (set _envPipeline pipeline) . renderDepthMap frameBuffer
   )
 where
  renderDepthMap :: GL.FramebufferObject -> Scene -> Render ()
  renderDepthMap frameBuffer Scene{..} = do
    pipeline <- asks envPipeline
    let Daylight{..} = sceneDaylight
    liftIO $ bindPipeline pipeline
    GL.bindFramebuffer GL.Framebuffer $= frameBuffer
    -- Set projection matrix
    projection <- liftIO $ toGlMatrix directionalLightProjection
    pipelineUniform pipeline "projectionM"
      $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    viewMatrix <- liftIO $ toGlMatrix . directionalLightViewMatrix daylightPitch
      $ daylightYaw
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set viewport to shadow map size
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size shadowMapWidth shadowMapWidth
      )
    liftIO $ GL.clear [GL.DepthBuffer]
    mapM_ renderElement sceneElements
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind

  renderElement :: Element -> Render ()
  renderElement Element{..} = do
    when elementShadow $ do
      let modelMatrix = mkTransformation elementRotation elementPosition
          globalTransforms = makeGlobalTransforms elementModel elementAnimation
          jointMatrices = fmap (makeJointMatrices globalTransforms) modelSkins
          Model{..} = elementModel
      V.zipWithM_ (renderNode modelMatrix jointMatrices) globalTransforms
        modelNodes

  renderNode :: M44 Float
    -> Vector (Vector (M44 Float))
    -> M44 Float
    -> Node
    -> Render ()
  renderNode modelMatrix jointMatricess bindMatrix Node{..} = do
    let jointMatrices = maybe mempty (jointMatricess V.!) nodeSkin
    mapM_ (mapM_ (renderMeshPrimitive modelMatrix bindMatrix jointMatrices))
      nodeMesh

  renderMeshPrimitive :: M44 Float
    -> M44 Float
    -> Vector (M44 Float)
    -> MeshPrimitive
    -> Render ()
  renderMeshPrimitive modelMatrix' bindMatrix' jointMatrices' MeshPrimitive{..} = do
    pipeline <- asks envPipeline
    let Material{..} = meshPrimMaterial
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
    -- Set model matrix
    modelMatrix <- liftIO $ toGlMatrix modelMatrix'
    pipelineUniform pipeline "modelM"
      $= (modelMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set bind matrix
    bindMatrix <- liftIO $ toGlMatrix bindMatrix'
    pipelineUniform pipeline "bindM"
      $= (bindMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set skinned uniform
    let skinned = not . null $ jointMatrices'
    pipelineUniform pipeline "skinned"
      $= (fromIntegral . fromEnum $ skinned :: GL.GLint)
    -- Set joint matrices
    liftIO $ pipelineUniformMatrix4v pipeline "jointM" jointMatrices'
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    liftIO $ GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    GL.bindVertexArrayObject $= Nothing
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
