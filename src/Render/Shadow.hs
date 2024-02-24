module Render.Shadow (
  createShadowDepthMapper
) where

import Data.StateVar
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import Linear as L

import App
import Camera as Cam
import Render.Matrix as M
import Render.Model
import Render.Pipeline

depthMapWidth, depthMapHeight, depthMapTextureImageLevel :: GL.GLsizei
depthMapWidth = 2048
depthMapHeight = 2048
depthMapTextureImageLevel = 0

-- Create a texture object and a callback that renders the shadow depth map
-- from the perspective of out light source to the texture.
createShadowDepthMapper :: [Model]
  -> IO (GL.TextureObject, WorldState -> IO ())
createShadowDepthMapper scene = do
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
  renderDepthMap :: GL.FramebufferObject -> Pipeline -> WorldState -> IO ()
  renderDepthMap frameBuffer pipeline WorldState{..} = do
    bindPipeline pipeline
    GL.bindFramebuffer GL.Framebuffer $= frameBuffer
    -- Set projection matrix
    projection <- M.toGlMatrix directionalLightProjection
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projectionUniform $= (projection :: GL.GLmatrix GL.GLfloat)
    -- Set view matrix
    viewMatrix <- M.toGlMatrix
      . directionalLightViewMatrix daylightDirection $ Cam.worldUp
    let viewUniform = pipelineUniform pipeline "viewM"
    viewUniform $= (viewMatrix :: GL.GLmatrix GL.GLfloat)
    -- Set model matrix
    model <- M.toGlMatrix (L.identity :: L.M44 GL.GLfloat)
    let modelUniform = pipelineUniform pipeline "modelM"
    modelUniform $= (model :: GL.GLmatrix GL.GLfloat)
    GL.viewport $= (
        GL.Position 0 0,
        GL.Size depthMapWidth depthMapHeight
      )
    GL.clear [GL.DepthBuffer]
    mapM_ (traverseModel_ (renderMeshPrimitive pipeline)) scene
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject -- unbind

  renderMeshPrimitive :: Pipeline -> M44 Float -> MeshPrimitive -> IO ()
  renderMeshPrimitive pipeline modelMatrix' MeshPrimitive{..} = do
    -- Set model matrix
    modelMatrix <- M.toGlMatrix modelMatrix'
    let modelMatrixUniform = pipelineUniform pipeline "modelM"
    modelMatrixUniform $= (modelMatrix :: GL.GLmatrix GL.GLfloat)
    -- Draw
    GL.bindVertexArrayObject $= Just meshPrimVao
    GL.drawElements meshPrimGlMode meshPrimNumIndices GL.UnsignedInt
      nullPtr
    GL.bindVertexArrayObject $= Nothing
