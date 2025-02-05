module App.Render.Text (
  Text,
  deleteText,

  createText,
  createTextRenderer,

  MsdfFont,
  loadFont
) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.StateVar
import qualified Data.IntMap.Strict as Map
import Foreign
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import App.Matrix
import App.Render.Env
import App.Render.Pipeline
import App.Render.Shaders
import App.Render.Text.Font.MSDF
import App.Render.Util

type Origin = (GL.GLfloat, GL.GLfloat)
type Scale = Float

data Text = Text {
  textFont :: MsdfFont, -- font atlas
  textVao :: GL.VertexArrayObject,
  textVbo :: GL.BufferObject,
  textEbo :: GL.BufferObject,
  textNumIndices :: GL.NumArrayIndices, -- convenience for draw commands
  textBackground :: TextBackground
}

data TextBackground = TextBackground {
  textBgVao :: GL.VertexArrayObject,
  textBgVbo :: GL.BufferObject,
  textBgNumVertices :: GL.NumArrayIndices
}

-- Create a renderer that renders text in "debug text space" which has -1 and 1
-- touching the edges of the longest axis of the viewport (in practice this
-- will be along the x-axis)
createTextRenderer :: IO (Text -> Bool -> Render ())
createTextRenderer = do
  textPipeline <- compilePipeline [
      ("text", VertexShader),
      ("text", FragmentShader)
    ]
  backgroundPipeline <- compilePipeline [
      ("text-background", VertexShader),
      ("text-background", FragmentShader)
    ]
  return $ \text background -> do
    when background $
      renderBackground backgroundPipeline text
    renderText textPipeline text
 where
  renderText :: Pipeline -> Text -> Render ()
  renderText pipeline Text{..} = do
    let MsdfFont{..} = textFont
    liftIO $ bindPipeline pipeline
    -- Set projection matrix
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projectionM <- liftIO . toGlMatrix =<< projection
    projectionUniform $= (projectionM :: GL.GLmatrix GL.GLfloat)
    -- Bind Texture
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texture
    -- Bind VAO
    GL.bindVertexArrayObject $= Just textVao
    -- Draw
    liftIO $ GL.drawElements GL.Triangles textNumIndices GL.UnsignedInt nullPtr
    -- Unbind
    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing

  renderBackground :: Pipeline -> Text -> Render ()
  renderBackground pipeline text = do
    let TextBackground{..} = textBackground text
    liftIO $ bindPipeline pipeline
    -- Set projection matrix
    -- TODO don't create the projection every render
    let projectionUniform = pipelineUniform pipeline "projectionM"
    projectionM <- liftIO . toGlMatrix =<< projection
    projectionUniform $= (projectionM :: GL.GLmatrix GL.GLfloat)
    -- Bind VAO
    GL.bindVertexArrayObject $= Just textBgVao
    -- Draw
    liftIO $ GL.drawArrays GL.Triangles 0 textBgNumVertices
    -- Unbind
    GL.bindVertexArrayObject $= Just textBgVao
    GL.bindVertexArrayObject $= Nothing

  projection :: Render (L.M44 Float)
  projection = do
    aspectRatio <- asks viewportAspectRatio
    return $ if aspectRatio > 1
      then let t = recip aspectRatio in L.ortho (-1) 1 (-t) t 1 (-1)
      else let r = -aspectRatio      in L.ortho (-r) r (-1) 1 1 (-1)

-- createDebugText - positioned in "text space" with ems as the unit
createText :: MsdfFont -> Scale -> Origin -> String -> Render Text
createText font@MsdfFont{..} scale' origin str = do
  let MsdfFontMeta{..} = meta
      sizeOfVertexUnit = sizeOf (undefined :: GL.GLfloat) :: Int
      glyphMap = unGlyphMap glyphs
  -- Create glyph quad vertices with the following layout
  -- Position  Texture co-ords
  -- x   y     x   y
      (quads, (cursorX, _)) = foldl accumGlyphQuads ([], origin)
        . mapMaybe (flip Map.lookup glyphMap . ord) $ str
      indices = glyphQuadIndices . (`div` (numVertexElements * 4))
                  . length $ quads
  -- Create VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Create VBO
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  -- Load vertices into VBO
  liftIO $ withArray quads $ \ptr -> do
    let size = fromIntegral . (* sizeOfVertexUnit) . length $ quads
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Create EBO
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  liftIO $ withArray indices $ \ptr -> do
    let size = fromIntegral . (* sizeOf (undefined :: GL.ArrayIndex))
                 . length $ indices
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
  textBackground <- createTextBackground scale' origin cursorX
  return $ Text {
      textFont = font,
      textVao = vao,
      textVbo = vbo,
      textEbo = ebo,
      textNumIndices = fromIntegral . length $ indices,
      textBackground = textBackground
    }
 where
  numVertexElements = 4

  accumGlyphQuads (vss, cursor) g =
    let (vs, cursor') = glyphQuad (metrics meta) scale' cursor g
    in (vss ++ vs, cursor')

  -- Create a quad for a glyph with the given horizontal offset and return the
  -- new offset for the cursor.
  glyphQuad :: FontMetrics
    -> Scale
    -> Origin
    -> Glyph
    -> ([GL.GLfloat], Origin)
  glyphQuad FontMetrics{..} s (x, y) Glyph{..} =
    let x' = x + s * advance
    -- TODO Create a default quad for unknown glyphs
    in (maybe [] vertices bounds, (x', y))
   where
    vertices :: GlyphBounds -> [GL.GLfloat]
    vertices GlyphBounds{..} =
      concat . zipWith (++) (vertexPositions planeBounds)
        . (textureCoordinates . atlas $ meta)
        $ atlasBounds

    vertexPositions :: Bounds -> [[GL.GLfloat]]
    vertexPositions Bounds{..} = [
        [x + s * left , y + s * (bottom - descender)],
        [x + s * right, y + s * (bottom - descender)],
        [x + s * left , y + s * (top    - descender)],
        [x + s * right, y + s * (top    - descender)]
      ]

    textureCoordinates :: FontAtlasMeta -> Bounds -> [[GL.GLfloat]]
    textureCoordinates FontAtlasMeta{..} Bounds{..} =
      let width'  = realToFrac width
          height' = realToFrac height
      in [
           [left  / width', bottom / height'],
           [right / width', bottom / height'],
           [left  / width', top    / height'],
           [right / width', top    / height']
         ]

  -- Generate the indices for n glyphs
  glyphQuadIndices :: Int -> [GL.ArrayIndex]
  glyphQuadIndices n = concatMap quadIndices [0..n-1]
   where
    quadIndices m = fmap ($ fromIntegral m * 4)
                         [id, (+1), (+2), (+1), (+2), (+3)]

createTextBackground :: Scale
  -> Origin
  -> GL.GLfloat
  -> Render TextBackground
createTextBackground scale' (x, y) x' = do
  let quad = [
          x , y ,
          x', y ,
          x , y',
          x', y ,
          x , y',
          x', y'
        ]
      numVertexElems = 2
      y' = y + scale'
      sizeOfVertexUnit = sizeOf (undefined :: GL.GLfloat) :: Int
  -- Create VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  -- Create VBO
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  -- Load vertices into VBO
  liftIO $ withArray quad $ \ptr -> do
    let size = fromIntegral . (* sizeOfVertexUnit) . length $ quad
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
  -- Create attribute pointers
  let posAttrLoc = GL.AttribLocation 0
      posAttrWidth = 2
      stride = 2
  GL.vertexAttribPointer posAttrLoc $=
    (GL.ToFloat,
     GL.VertexArrayDescriptor
       posAttrWidth
       GL.Float
       (fromIntegral sizeOfVertexUnit * stride)
       nullPtr
     )
  -- Unbind VAO and VBO
  GL.vertexAttribArray posAttrLoc $= GL.Enabled
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  return $ TextBackground {
      textBgVao = vao,
      textBgVbo = vbo,
      textBgNumVertices = fromIntegral $ length quad `div` numVertexElems
    }

deleteText :: Text -> Render ()
deleteText Text{..} = do
  GL.deleteObjectName textVbo
  GL.deleteObjectName textEbo
  GL.deleteObjectName textVao
  let TextBackground{..} = textBackground
  GL.deleteObjectName textBgVbo
  GL.deleteObjectName textBgVao
  return ()
