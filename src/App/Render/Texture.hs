module App.Render.Texture (
  TextureWrapMode(..),

  GL.TextureFilter(..),
  GL.MinificationFilter,
  GL.MagnificationFilter,

  SRGB,

  readImage,
  decodeImage,
  fromImage,

  missingImageData
) where

import Codec.Picture hiding (readImage, decodeImage)
import qualified Codec.Picture as P
import Codec.Picture.Extra
import Codec.Picture.Types
import Control.Monad
import Data.ByteString as BS
import Data.Maybe
import Data.StateVar
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Unsafe
import Text.Printf

-- Wether a loaded texture should be linearised
type SRGB = Bool

type TextureWrapModeS = TextureWrapMode
type TextureWrapModeT = TextureWrapMode
data TextureWrapMode = ClampToEdge | MirroredRepeat | Repeat

{-# NOINLINE missingImageData #-}
missingImageData :: ByteString
missingImageData = unsafePerformIO $ do
  putStrLn "Reading default image texture."
  BS.readFile "assets/textures/missing-texture.png"

-- Loads a 2D texture from an image file
readImage :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> FilePath
  -> IO GL.TextureObject
readImage l minF magF wrapS wrapT filePath = do
  image <- P.readImage filePath
  -- TODO handle failure
  fromDynamicImage l minF magF wrapS wrapT . dynamicPixelMap flipVertically
    . either error id $ image

-- Load a 2D texture from a Bytestring
decodeImage :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> ByteString
  -> IO GL.TextureObject
decodeImage srgb minF magF wrapS wrapT bytes = do
  let image = P.decodeImage bytes
  fromDynamicImage srgb minF magF wrapS wrapT . either error id $ image

fromDynamicImage :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> DynamicImage
  -> IO GL.TextureObject
fromDynamicImage srgb minF magF wrapS wrapT dynamicImage = do
  case dynamicImage of
    -- Pixel formats convertable to RGB
    ImageRGB8   i -> fromImage' i
    ImageYCbCr8 i -> (fromImage' :: Image PixelRGB8 -> IO GL.TextureObject)
                       . convertImage $ i
    ImageCMYK8  i -> (fromImage' :: Image PixelRGB8 -> IO GL.TextureObject)
                       . convertImage $ i
    -- RGBA
    ImageRGBA8  i -> fromImage srgb minF magF wrapS wrapT i
    -- Missing support for the following pixel formats
    ImageY8 _ -> reportUnsupported "Pixel8"
    ImageY16 _ -> reportUnsupported "Pixel16"
    ImageY32 _ -> reportUnsupported "Pixel32"
    ImageYF _ -> reportUnsupported "PixelF"
    ImageYA8 _ -> reportUnsupported "PixelYA8"
    ImageYA16 _ -> reportUnsupported "PixelYA16"
    ImageRGB16 _ -> reportUnsupported "PixelRGB16"
    ImageRGBF _ -> reportUnsupported "PixelRGBF"
    ImageRGBA16 _ -> reportUnsupported "PixelRGB16"
    ImageCMYK16 _ -> reportUnsupported "PixelCMYK16"
 where
  reportUnsupported :: String -> a
  reportUnsupported = error
    . printf "fromDynamicImage: unsupported pixel format: %s"

  fromImage' = fromImage srgb minF magF wrapS wrapT

class GlPixel a where
  glPixelFormat :: GL.PixelFormat
  glPixelInternalFormat :: GL.PixelInternalFormat
  glPixelSRGBInternalFormat :: GL.PixelInternalFormat

instance GlPixel PixelRGB8 where
  glPixelFormat = GL.RGB
  glPixelInternalFormat = GL.RGB'
  glPixelSRGBInternalFormat = GL.SRGB

instance GlPixel PixelRGBA8 where
  glPixelFormat = GL.RGBA
  glPixelInternalFormat = GL.RGBA'
  glPixelSRGBInternalFormat = GL.SRGBAlpha

fromImage :: (Pixel a, GlPixel a) => SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> Image a
  -> IO GL.TextureObject
fromImage srgb minF magF wrapS wrapT image = do
  -- Create the texture object
  texture <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= (minF, magF)
  GL.textureWrapMode GL.Texture2D GL.S $= toGlWrapMode wrapS
  GL.textureWrapMode GL.Texture2D GL.S $= toGlWrapMode wrapT
  texImage2D srgb image
  -- Mipmap generation
  when (isJust . snd $ minF) $ GL.generateMipmap' GL.Texture2D
  -- Unbind the texture
  GL.textureBinding GL.Texture2D $= Nothing
  return texture
 where
  toGlWrapMode :: TextureWrapMode -> (GL.Repetition, GL.Clamping)
  toGlWrapMode mode = case mode of
    ClampToEdge    -> (GL.Repeated, GL.ClampToEdge)
    MirroredRepeat -> (GL.Mirrored, GL.Repeat)
    Repeat         -> (GL.Repeated, GL.Repeat)

texImage2D :: forall a. (Pixel a, GlPixel a)
  => Bool
  -> Image a
  -> IO ()
texImage2D srgb image = do
  let width = fromIntegral . imageWidth $ image
      height = fromIntegral . imageHeight $ image
  V.unsafeWith (imageData image) $ \ptr ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      (if srgb then glPixelSRGBInternalFormat @a else glPixelInternalFormat @a)
      (GL.TextureSize2D width height)
      0
      (GL.PixelData (glPixelFormat @a) GL.UnsignedByte (castPtr ptr))

