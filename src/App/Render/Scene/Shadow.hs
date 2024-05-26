module App.Render.Scene.Shadow (
  createShadowMapper
) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.StateVar
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import App.Entity
import App.Matrix
import App.Projection
import App.Render.Env
import App.Render.Model
import App.Render.Pipeline
import App.Render.Scene.Entity
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
  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D
    shadowMap shadowMapTextureImageLevel
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
    aspectRatio <- asks viewportAspectRatio
    let Daylight{..} = sceneDaylight
    liftIO . bindPipeline $ pipeline
    -- Bind output to frame buffer
    GL.bindFramebuffer GL.Framebuffer $= frameBuffer
    -- Set view matrix
    viewMatrix <- liftIO . toGlMatrix
      . directionalLightViewMatrix sceneCamera daylightPitch daylightYaw
      $ aspectRatio
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set projection matrix
    projection <- liftIO $ toGlMatrix
      . directionalLightProjection sceneCamera daylightPitch daylightYaw
      $ aspectRatio
    pipelineUniform pipeline "projectionM"
      $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set viewport to shadow map size
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size shadowMapWidth shadowMapWidth
      )
    -- Clear depth buffer
    liftIO $ GL.clear [GL.DepthBuffer]
    mapM_ (uncurry renderInstances) . M.assocs $ sceneEntities
    -- Unbind framebuffer
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

renderInstances :: Entity -> [Instance] -> Render ()
renderInstances entity instances = do
  Model{..} <- asks (entityModel entity . envModels)
  -- Buffer instance model matrices
  bufferData 0 . SV.fromList . fmap (transpose . instanceModelMatrix)
    $ instances
  -- Buffer instance transformation matrices
  bufferData 1 . SV.convert . V.concat
    . fmap (fmap transpose . instanceTransformationMatrices) $ instances
  -- Buffer skin offsets
  bufferData 2 . SV.convert . fmap (V.length . skinJoints) $ modelSkins
  -- Buffer instance joint matrices
  bufferData 3 . SV.convert . V.concat
    . fmap (fmap transpose . fold . instanceJointMatrices) $ instances
  GL.bindBuffer GL.ShaderStorageBuffer $= Nothing
  -- Render meshes
  shouldRenderMeshes <- asks envRenderMeshes
  let numInstances = length instances
      numNodes = V.length modelNodes
      numJoints = V.length . foldMap skinJoints $ modelSkins
  when (entityHasShadow entity && shouldRenderMeshes) $
    imapM_ (renderModelNode numInstances numNodes numJoints) modelNodes
 where
  bufferData :: (Storable a) => Int -> SV.Vector a -> Render ()
  bufferData bindingIndex dat = do
    buffer <- GL.genObjectName
    GL.bindBuffer GL.ShaderStorageBuffer $= Just buffer
    liftIO . SV.unsafeWith dat $ \ptr -> do
      let size = fromIntegral . (* SV.length dat) . sizeOf
                   $ (undefined :: ModelMatrix)
      GL.bufferData GL.ShaderStorageBuffer
        $= (size, ptr, GL.StaticDraw)
    (GL.bindBufferBase GL.IndexedShaderStorageBuffer
      . fromIntegral $ bindingIndex) $= Just buffer

renderModelNode :: Int -> Int -> Int -> Int -> Node -> Render ()
renderModelNode numinstances numNodes numJoints nodeId Node{..} = do
  let skinId = fromMaybe (-1) nodeSkin
  mapM_ (renderMeshPrimitive numinstances numNodes numJoints nodeId skinId)
        nodeMesh

renderMeshPrimitive :: Int -> Int -> Int -> Int -> Int -> MeshPrimitive -> Render ()
renderMeshPrimitive numInstances numNodes numJoints nodeId skinId meshPrim = do
  pipeline <- asks envPipeline
  let Material{..} = meshPrimMaterial meshPrim
  -- Number of model nodes + node id of mesh (for offset calculations)
  pipelineUniform pipeline "numNodes" $= (fromIntegral numNodes :: GL.GLint)
  pipelineUniform pipeline "nodeId" $= (fromIntegral nodeId :: GL.GLint)
  -- Skin ID and number of joints in model
  pipelineUniform pipeline "skinId" $= (fromIntegral skinId :: GL.GLint)
  pipelineUniform pipeline "numJoints" $= (fromIntegral numJoints :: GL.GLint)
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
  GL.bindVertexArrayObject $= (Just . meshPrimVao $ meshPrim)
  liftIO $ GL.drawElementsInstanced (meshPrimGlMode meshPrim)
                                    (meshPrimNumIndices meshPrim)
                                    GL.UnsignedInt
                                    nullPtr
                                    (fromIntegral numInstances)
  GL.bindVertexArrayObject $= Nothing
  -- Unbind
  GL.bindVertexArrayObject $= Nothing
  GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
  GL.textureBinding GL.Texture2D $= Nothing
