module App.Render.Scene (
  module App.Render.Scene.Scene,

  createSceneRenderer
) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.StateVar
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import App.Camera as Cam
import App.Matrix
import App.Render.Env
import App.Render.Model
import App.Render.Pipeline
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
    -- Set ambient intensity
    pipelineUniform pipeline "ambientIntensity" $= daylightAmbientIntensity
    -- Set light direction
    pipelineUniform pipeline "lightDirection" $=
      (toGlVector3 . eulerDirection daylightPitch $ daylightYaw)
    -- Set camera position
    pipelineUniform pipeline "camPos" $= (toGlVector3 . camPos $ sceneCamera)
    -- Render
    width <- asks (fromIntegral . envViewportWidth)
    height <- asks (fromIntegral . envViewportHeight)
    GL.viewport $= (GL.Position 0 0, GL.Size width height)
    liftIO . GL.clear $ [GL.ColorBuffer, GL.DepthBuffer]
    mapM_ renderElement sceneElements
    -- Unbind textures
    GL.activeTexture $= GL.TextureUnit shadowMapTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing

  renderElement :: Element -> Render ()
  renderElement Element{..} = do
    -- Render meshes
    let modelMatrix = mkTransformation elementRotation elementPosition
        nodeTransforms = makeGlobalTransforms elementModel elementAnimation
        -- Indexed by skin
        jointMatrices = fmap (makeJointMatrices nodeTransforms) . modelSkins $ elementModel
    shouldRenderMeshes <- asks envRenderMeshes
    when shouldRenderMeshes $
      V.zipWithM_ (renderModelNode modelMatrix jointMatrices) nodeTransforms
        . modelNodes $ elementModel
    -- Render joints
    showJoints <- asks envShowJoints
    -- Extract the translation from the model matrix
    when showJoints
      . V.zipWithM_ (renderJointNode modelMatrix) nodeTransforms
      . modelNodes $ elementModel

  renderModelNode :: M44 Float
    -> Vector (Vector (M44 Float))
    -> M44 Float
    -> Node
    -> Render ()
  renderModelNode modelMatrix jointMatricess bindMatrix Node{..} = do
    let jointMatrices = maybe mempty (jointMatricess V.!) nodeSkin
    mapM_ (mapM_ (renderMeshPrimitive modelMatrix bindMatrix jointMatrices))
      nodeMesh

  renderJointNode :: M44 Float -> M44 Float -> Node -> Render ()
  renderJointNode modelMatrix bindMatrix Node{..} = do
    when nodeIsJoint $ do
      jointModel <- asks envJointModel
      -- Render the joint model at the origin of the joint.
      let modelMatrix' = translate . (^. _xyz)
            $ modelMatrix !*! bindMatrix !* V4 0 0 0 1
          nodeTransforms = makeGlobalTransforms jointModel Nothing
      V.zipWithM_ (renderModelNode modelMatrix' mempty)
          nodeTransforms
        . modelNodes $ jointModel

  renderMeshPrimitive :: M44 Float
    -> M44 Float
    -> Vector (M44 Float)
    -> MeshPrimitive
    -> Render ()
  renderMeshPrimitive modelMatrix' bindMatrix' jointMatrices' MeshPrimitive{..} = do
    pipeline <- asks envPipeline
    let Material{..} = meshPrimMaterial
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
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.activeTexture $= GL.TextureUnit baseColorTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit metallicRoughnessTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
    GL.activeTexture $= GL.TextureUnit normalTextureUnit
    GL.textureBinding GL.Texture2D $= Nothing
