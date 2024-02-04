module Render.Text.Font.MSDF (
  MsdfFont(..),
  MsdfFontMeta(..),
  FontAtlasMeta(..),
  FontAtlasTextureType(..),
  FontMetrics(..),
  GlyphMap(..),
  Glyph(..),
  GlyphBounds(..),
  Bounds(..),

  loadFont
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.IntMap.Strict
import qualified Data.Vector as V
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL

import Render.Texture

type TextureWidth = Int;
type TextureHeight = Int;

data MsdfFont = MsdfFont {
  -- Indexed by unicode number
  meta:: MsdfFontMeta,
  texture :: GL.TextureObject
}

data MsdfFontMeta = MsdfFontMeta {
  atlas :: FontAtlasMeta,
  metrics :: FontMetrics,
  glyphs :: GlyphMap
} deriving (Show, Generic)

instance FromJSON MsdfFontMeta
 
data FontAtlasMeta = FontAtlasMeta {
  atlasType :: FontAtlasTextureType,
  distanceRange :: Int,
  size :: Float,
  width :: TextureWidth,
  height :: TextureHeight
} deriving (Show, Generic)

instance FromJSON FontAtlasMeta where
  parseJSON = withObject "FontAtlasMeta" $ \v -> FontAtlasMeta
    <$> v .: "type"
    <*> v .: "distanceRange"
    <*> v .: "size"
    <*> v .: "width"
    <*> v .: "height"

data FontAtlasTextureType = MTSDF | MSDF
  deriving (Show)

instance FromJSON FontAtlasTextureType where
  parseJSON = withText "TextureType" parseTextureType
   where
    parseTextureType t | t == "mtsdf" = return MTSDF
                       | t == "msdf"  = return MSDF
                       | otherwise    = parseFail "Unrecognised atlas type" 

data FontMetrics = FontMetrics {
  emSize :: Int,
  lineHeight :: Float,
  ascender :: Float,
  descender :: Float,
  underlineY :: Float,
  underlineThickness :: Float
} deriving (Show, Generic)

instance FromJSON FontMetrics

newtype GlyphMap = GlyphMap { unGlyphMap :: IntMap Glyph }
  deriving (Show)

instance FromJSON GlyphMap where
  parseJSON = withArray "GlyphMap" $ \a -> do
    gs <- fmap V.toList . mapM parseJSON $ a
    return . GlyphMap . fromList . fmap ((,) <$> unicode <*> id) $ gs

data Glyph = Glyph {
  unicode :: Int,
  advance :: Float,
  bounds :: Maybe GlyphBounds
} deriving (Show)

instance FromJSON Glyph where
  parseJSON = withObject "Glyph" $ \v -> Glyph
    <$> v .: "unicode"
    <*> v .: "advance"
    <*> parseBounds v
   where
    parseBounds :: Object -> Parser (Maybe GlyphBounds)
    parseBounds v = do
      atlas <- v .:? "atlasBounds"
      plane <- v .:? "planeBounds"
      when (isJust atlas && isNothing plane)
        . fail $ "Glyph: expecting planeBounds"
      when (isNothing atlas && isJust plane)
        . fail $ "Glyph: expecting atlasBounds"
      return $ GlyphBounds <$> atlas <*> plane

data GlyphBounds = GlyphBounds {
  atlasBounds :: Bounds,
  planeBounds :: Bounds
} deriving (Show)

data Bounds = Bounds {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
} deriving (Show, Generic)

instance FromJSON Bounds

loadFont :: FilePath -> IO MsdfFont
loadFont fontName = do
  let fontPath = "assets/fonts/" ++ fontName
  content <- LBS.readFile $ fontPath ++ ".json"
  let fontMeta = fromMaybe (error $ "Failed to parse font " ++ fontName)
        . decode $ content
      texturePath = fontPath ++ ".png"
  texture <- loadTexture texturePath (Linear', Nothing) Linear' ClampToEdge
  return $ MsdfFont fontMeta texture
