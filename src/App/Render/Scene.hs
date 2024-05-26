module App.Render.Scene (
  module App.Render.Scene.Scene,

  createSceneRenderer
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

import App.Camera as Cam
import App.Entity
import App.Matrix
import App.Projection
import App.Render.Env
import App.Render.Model
import App.Render.Pipeline
import App.Render.Scene.Entity
import App.Render.Scene.Scene
import App.Vector

shadowMapTextureUnit :: Integral a => a
shadowMapTextureUnit = 0

baseColorTextureUnit :: Integral a => a
baseColorTextureUnit = 1

metallicRoughnessTextureUnit :: Integral a => a
metallicRoughnessTextureUnit = 2

normalTextureUnit :: Integral a => a
normalTextureUnit = 3

createSceneRenderer :: GL.TextureObject
  -> IO (Scene -> Render ())
createSceneRenderer shadowDepthMap = do
  pipeline <- compilePipeline [
      ("shader", GL.FragmentShader),
      ("shader", GL.VertexShader)
    ]
  return $ local (set _envPipeline pipeline) . render
 where
  render :: Scene -> Render ()
  render Scene{..} = do
    pipeline <- asks envPipeline
    let Daylight{..} = sceneDaylight
    liftIO . bindPipeline $ pipeline
    -- Bind the shadow map texture
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Just shadowDepthMap
    -- Set view matrix
    viewMatrix <- liftIO . toGlMatrix .  Cam.toViewMatrix $ sceneCamera
    pipelineUniform pipeline "viewM" $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set projection matrix
    aspectRatio <- asks viewportAspectRatio
    projection <- liftIO . toGlMatrix . perspectiveProjection $ aspectRatio
    pipelineUniform pipeline "projectionM" $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set light projection matrix
    lightProjection <- liftIO . toGlMatrix
      . directionalLightProjection sceneCamera daylightPitch daylightYaw
      $ aspectRatio
    pipelineUniform pipeline "lightProjectionM"
      $= (lightProjection :: GL.GLmatrix GL.GLfloat)
    -- Set light view matrix
    lightView <- liftIO . toGlMatrix
      . directionalLightViewMatrix sceneCamera daylightPitch daylightYaw
      $ aspectRatio
    pipelineUniform pipeline "lightViewM"
      $= (lightView :: GL.GLmatrix GL.GLfloat)
    -- Set daylight
    pipelineUniform pipeline "daylightColor" $= toGlVector3 daylightColor
    pipelineUniform pipeline "daylightIntensity" $= daylightIntensity
    -- Set light direction
    pipelineUniform pipeline "lightDirection" $=
      (toGlVector3 . eulerDirection daylightPitch $ daylightYaw)
    -- Set camera position
    pipelineUniform pipeline "camPos" $= (toGlVector3 . camPos $ sceneCamera)
    -- Render
    width <- asks (fromIntegral . envViewportWidth)
    height <- asks (fromIntegral . envViewportHeight)
    -- Set viewport
    GL.viewport $= (GL.Position 0 0, GL.Size width height)
    -- Clear buffers
    liftIO . GL.clear $ [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ (uncurry renderInstances) . M.assocs $ sceneEntities
    -- Unbind textures
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing

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
  when shouldRenderMeshes $
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
  -- Base color
  pipelineUniform pipeline "baseColorFactor"
    $= toGlVector4 materialBaseColorFactor
  pipelineUniform pipeline "hasBaseColorTexture"
    $= maybe (0 :: GL.GLint) (const 1) materialBaseColorTexture
  GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
  GL.textureBinding GL.Texture2D $= materialBaseColorTexture
  -- Metallic/roughness
  pipelineUniform pipeline "metallicFactor" $= materialMetallicFactor
  pipelineUniform pipeline "roughnessFactor" $= materialRoughnessFactor
  pipelineUniform pipeline "hasMetallicRoughnessTexture"
    $= maybe (0 :: GL.GLint) (const 1) materialMetallicRoughnessTexture
  GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
  GL.textureBinding GL.Texture2D $= materialMetallicRoughnessTexture
  -- Normal mapping
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
  -- Draw
  GL.bindVertexArrayObject $= (Just . meshPrimVao $ meshPrim)
  liftIO $ GL.drawElementsInstanced (meshPrimGlMode meshPrim)
                                    (meshPrimNumIndices meshPrim)
                                    GL.UnsignedInt
                                    nullPtr
                                    (fromIntegral numInstances)
  -- Unbind
  GL.bindVertexArrayObject $= Nothing
  GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
  GL.textureBinding GL.Texture2D $= Nothing
  GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
  GL.textureBinding GL.Texture2D $= Nothing
  GL.activeTexture $= GL.TextureUnit normalTextureUnit
  GL.textureBinding GL.Texture2D $= Nothing
