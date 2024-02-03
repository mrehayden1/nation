module Text (
  Text,
  deleteText,

  createDebugText,
  createDebugTextRenderer,

  MsdfFont,
  loadFont
) where

import Data.Char
import Data.Maybe
import Data.StateVar
import qualified Data.IntMap.Strict as Map
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import qualified Matrix as M

import Element
import Pipeline
import Shaders
import Text.Font.MSDF
import Util

type Origin = (VertexUnit, VertexUnit)
type Scale = Float

data Text = Text {
  textFont :: MsdfFont, -- font atlas
  textVao :: GL.VertexArrayObject,
  textVbo :: GL.BufferObject,
  textEbo :: GL.BufferObject,
  textEboIndices :: GL.NumArrayIndices -- convenience for draw commands
}

-- Create a renderer that renders text in "debug text space" which has -1 and 1
-- touching the left and right window edges respectively along the longest axis
-- of the viewport (in practice this will be along the x-axis)
createDebugTextRenderer :: Float -> IO (Text -> IO ())
createDebugTextRenderer aspectRatio = do
  pipeline <- createPipeline [
      ("text", VertexShader),
      ("text", FragmentShader)
    ]
  return $ renderText pipeline
 where
  renderText :: Pipeline -> Text -> IO ()
  renderText pipeline Text{..} = do
    let MsdfFont{..} = textFont
    GL.currentProgram $= (Just . pipelineProgram $ pipeline)
    -- Set projection matrix
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projection' <- GL.newMatrix GL.RowMajor . M.unpack $ projection
    GL.uniform projectionUniform $= (projection' :: GL.GLmatrix GL.GLfloat)
    -- Bind Texture
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texture
    -- Bind VAO
    GL.bindVertexArrayObject $= Just textVao
    -- Draw
    GL.drawElements GL.Triangles textEboIndices GL.UnsignedInt nullPtr
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing
    return ()

  projection :: L.M44 Float
  projection =
    if aspectRatio > 1
      then let t = recip aspectRatio in L.ortho (-1) 1 (-t) t 1 (-1)
      else let r = -aspectRatio      in L.ortho (-r) r (-1) 1 1 (-1)
  

-- createDebugText - positioned in "text space" with ems as the unit
createDebugText :: MsdfFont -> Scale -> Origin -> String -> IO Text
createDebugText font@MsdfFont{..} scale origin str = do
  let MsdfFontMeta{..} = meta
      sizeOfVertexUnit = sizeOf (undefined :: VertexUnit) :: Int
      glyphMap = unGlyphMap glyphs
  -- Create glyph quad vertices with the following layout
  -- Position  Texture co-ords
  -- x   y     x   y
      quads = fst . foldl accum  ([], origin)
        . mapMaybe (flip Map.lookup glyphMap . ord) $ str
      indices = quadIndices . (`div` 4) . length $ quads
  -- Create VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Create VBO
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  -- Load vertices into VBO
  withArray quads $ \ptr -> do
    let size = fromIntegral . (* sizeOfVertexUnit) . length $ quads
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Create EBO
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  withArray indices $ \ptr -> do
    let size = fromIntegral . (* sizeOf (undefined :: GL.ArrayIndex))
                 . length $ quads
    GL.bufferData GL.ElementArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Define vertex attribute pointers
  let posAttrLoc = GL.AttribLocation 0
      posAttrWidth = 2
      texCoordAttrLoc = GL.AttribLocation 1
      texCoordAttrWidth = 2
      stride = 4
  GL.vertexAttribPointer posAttrLoc $=
    (GL.ToFloat,
     GL.VertexArrayDescriptor
       posAttrWidth
       GL.Float
       (fromIntegral sizeOfVertexUnit * stride)
       nullPtr
     )
  GL.vertexAttribArray posAttrLoc $= GL.Enabled
  GL.vertexAttribPointer texCoordAttrLoc $=
    (GL.ToFloat,
     GL.VertexArrayDescriptor
         texCoordAttrWidth GL.Float (fromIntegral sizeOfVertexUnit * stride)
       . bufferOffset
       $ (fromIntegral sizeOfVertexUnit * posAttrWidth)
    )
  GL.vertexAttribArray texCoordAttrLoc $= GL.Enabled
  -- Unbind VAO, VBO and EBO
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  return $ Text {
      textFont = font,
      textVao = vao,
      textVbo = vbo,
      textEbo = ebo,
      textEboIndices = fromIntegral . length $ indices
    }
 where
  accum (vss, o) g =
    let (vs, o') = glyphQuad (metrics meta) scale o g
    in (vss ++ vs, o')

  -- Create a quad for a glyph with the given horizontal offset and return the
  -- new offset for the cursor.
  glyphQuad :: FontMetrics -> Scale -> Origin -> Glyph -> (Vertices, Origin)
  glyphQuad FontMetrics{..} s (x, y) Glyph{..} =
    let x' = x + s * advance
    in (maybe [] vertices bounds, (x', y))
   where
    vertices :: GlyphBounds -> [VertexUnit]
    vertices GlyphBounds{..} =
      concat . zipWith (++) (vertexPositions planeBounds)
        . (textureCoordinates . atlas $ meta)
        $ atlasBounds

    vertexPositions :: Bounds -> [Vertices]
    vertexPositions Bounds{..} = [
        [x + s * left , y + s * (bottom - descender)],
        [x + s * right, y + s * (bottom - descender)],
        [x + s * left , y + s * (top    - descender)],
        [x + s * right, y + s * (top    - descender)]
      ]

    textureCoordinates :: FontAtlasMeta -> Bounds -> [Vertices]
    textureCoordinates FontAtlasMeta{..} Bounds{..} =
      let width'  = realToFrac width
          height' = realToFrac height
      in [
           [left  / width', bottom / height'],
           [right / width', bottom / height'],
           [left  / width', top    / height'],
           [right / width', top    / height']
         ]

  quadIndices :: Int -> [GL.ArrayIndex]
  quadIndices n = concatMap (quadIndices' . (* 4)) [0..n-1]
   where
    quadIndices' m = fmap ($ fromIntegral m) [id, (+1), (+2), (+1), (+2), (+3)]

deleteText :: Text -> IO ()
deleteText Text {..} = do
  GL.deleteObjectName textVbo
  GL.deleteObjectName textEbo
  GL.deleteObjectName textVao
  return ()
