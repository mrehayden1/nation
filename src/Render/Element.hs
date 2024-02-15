module Render.Element (
  RenderableElement(..),

  VertexUnit,
  Vertices,

  Index,

  createSceneElements,
  renderElement
) where

import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Render.Util

type VertexUnit = GL.GLfloat

type Index = GL.GLuint

{- Vertices are defiend as a tightly packed list of numbers with the following
 - layout for each vertex:
 -      position        normal
 -        x    y    z    nx   ny   nz
 - e.g. 1.0  1.0  1.0   1.0  1.0  1.0
 -
 - this is the format that our OpenGL binding expects with the layout defined
 - by our vertex attribute pointers
 - -}
type Vertices = [VertexUnit]

{- Graphics elements are either just vertices or vertices and indices. Index
 - elements are rendered as triangles.
 -}
data ElementSpec =
    VertexElement GL.PrimitiveMode Vertices
  | IndexedElement GL.PrimitiveMode Vertices [Index]

{- Stores pointers to Vertex Array Objects and no. of vertices / indices which
 - OpenGL needs to draw simple vertex elements and indexed elements
 - respectively.
 - -}
data RenderableElement =
   RenderableVertices GL.PrimitiveMode GL.VertexArrayObject GL.NumArrayIndices
 | RenderableIndexed GL.PrimitiveMode GL.VertexArrayObject GL.NumArrayIndices

stride :: Integral a => a
stride = 6

cube :: ElementSpec
cube =
  let vertices = [
      -- position          normal vector
      --   x     y     z    nx    ny    nz
        -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
         0.5, -0.5, -0.5,  0.0,  0.0, -1.0, 
         0.5,  0.5, -0.5,  0.0,  0.0, -1.0, 
         0.5,  0.5, -0.5,  0.0,  0.0, -1.0, 
        -0.5,  0.5, -0.5,  0.0,  0.0, -1.0, 
        -0.5, -0.5, -0.5,  0.0,  0.0, -1.0, 

        -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,
         0.5, -0.5,  0.5,  0.0,  0.0,  1.0,
         0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
         0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
        -0.5,  0.5,  0.5,  0.0,  0.0,  1.0,
        -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,

        -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,
        -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
        -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,
        -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,

         0.5,  0.5,  0.5,  1.0,  0.0,  0.0,
         0.5,  0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
         0.5, -0.5,  0.5,  1.0,  0.0,  0.0,
         0.5,  0.5,  0.5,  1.0,  0.0,  0.0,

        -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
         0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
         0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
         0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
        -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
        -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,

        -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
         0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
         0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
         0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
        -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
        -0.5,  0.5, -0.5,  0.0,  1.0,  0.0]
  in VertexElement GL.Triangles vertices

plane :: ElementSpec
plane =
  let vertices = [
         10, -1, -10,  0,  1,  0,
         10, -1,  10,  0,  1,  0,
        -10, -1,  10,  0,  1,  0,
        -10, -1, -10,  0,  1,  0]
      indices = [0, 1, 2, 0, 2, 3]
  in IndexedElement GL.Triangles vertices indices

createSceneElements :: IO [RenderableElement]
createSceneElements = mapM createElement [plane]

createElement :: ElementSpec -> IO RenderableElement
createElement spec = do
  let (primitiveMode, vertices, mIndices) = case spec of
        VertexElement  pm vs    -> (pm, vs, Nothing)
        IndexedElement pm vs is -> (pm, vs, Just is)
  -- Create and bind vertex array object before our vertex and element buffers
  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject
  -- Create and bind vertex buffer object
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  -- Load vertices into array buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral $ length vertices * sizeOf (0.0 :: GL.GLfloat)
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Create and bind element buffer object and load indices if required
  case mIndices of
    Just indices -> do
      elementBuffer <- GL.genObjectName
      GL.bindBuffer GL.ElementArrayBuffer $= Just elementBuffer
      withArray indices $ \ptr -> do
        let size = fromIntegral (length indices * sizeOf (0 :: GL.GLuint))
        GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
    Nothing      -> return ()
  -- Define vertex attribute pointers
  let sizeOfVertexUnit = fromIntegral . sizeOf $ (0.0 :: VertexUnit)
      positionAttributeLocation = GL.AttribLocation 0
      normalAttributeLocation   = GL.AttribLocation 1
  GL.vertexAttribPointer positionAttributeLocation $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (sizeOfVertexUnit * stride) nullPtr)
  GL.vertexAttribArray positionAttributeLocation $= GL.Enabled
  GL.vertexAttribPointer normalAttributeLocation $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (sizeOfVertexUnit * stride) . bufferOffset $ (sizeOfVertexUnit * 3))
  GL.vertexAttribArray normalAttributeLocation $= GL.Enabled
  -- Unbind the vertex array object
  GL.bindVertexArrayObject $= Nothing
  return $ case mIndices of
    Just indices -> RenderableIndexed primitiveMode vertexArrayObject
                      . fromIntegral . length $ indices
    Nothing      -> RenderableVertices primitiveMode vertexArrayObject
                      . (`div` stride) . fromIntegral . length $ vertices

renderElement :: RenderableElement -> IO ()
renderElement (RenderableVertices primitveMode vertexArrayObject vertices) = do
  GL.bindVertexArrayObject $= Just vertexArrayObject
  GL.drawArrays primitveMode 0 vertices
  GL.bindVertexArrayObject $= Nothing
renderElement (RenderableIndexed primitiveMode vertexArrayObject indices) = do
  GL.bindVertexArrayObject $= Just vertexArrayObject
  GL.drawElements primitiveMode indices GL.UnsignedInt nullPtr
  GL.bindVertexArrayObject $= Nothing
