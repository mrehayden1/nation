module Render.Texture (
  TextureWrapMode(..),

  GL.TextureFilter(..),
  GL.MinificationFilter,
  GL.MagnificationFilter,

  readTexture,
  loadTexture
) where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Control.Monad
import Data.ByteString
import Data.Maybe
import Data.StateVar
import Data.Word
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import GHC.Int
import qualified Graphics.Rendering.OpenGL as GL

type SRGB = Bool

type ImageWidth = Int32
type ImageHeight = Int32

type TextureWrapModeS = TextureWrapMode
type TextureWrapModeT = TextureWrapMode
data TextureWrapMode = ClampToEdge | MirroredRepeat | Repeat 
  -- | Border Float Float Float

class Linearisable a where
  linearise :: a -> a

instance Linearisable Word8 where
  linearise p = round $ (realToFrac p / 255 :: Float) ** 2.2

instance Linearisable PixelRGB8 where
  linearise (PixelRGB8 r g b) =
    PixelRGB8 (linearise r) (linearise g) (linearise b)

instance Linearisable PixelRGBA8 where
  linearise (PixelRGBA8 r g b a) =
    PixelRGBA8 (linearise r) (linearise g) (linearise b) a

-- Loads a 2D texture from an image file
readTexture :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> FilePath
  -> IO GL.TextureObject
readTexture l minF magF wrapS wrapT filePath = do
  image <- readImage filePath
  -- TODO handle failure
  loadTexture' l minF magF wrapS wrapT . dynamicPixelMap flipVertically
    . either error id $ image

-- Load a 2D texture from a Bytestring
loadTexture :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> ByteString
  -> IO GL.TextureObject
loadTexture srgb minF magF wrapS wrapT bytes = do
  let image = decodeImage bytes
  loadTexture' srgb minF magF wrapS wrapT . either error id $ image

loadTexture' :: SRGB
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapModeS
  -> TextureWrapModeT
  -> DynamicImage
  -> IO GL.TextureObject
loadTexture' srgb minF magF wrapS wrapT dynamicImage = do
  -- Create the texture object
  texture <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= (minF, magF)
  GL.textureWrapMode GL.Texture2D GL.S $= toGlWrapMode wrapS
  GL.textureWrapMode GL.Texture2D GL.S $= toGlWrapMode wrapT
  let width = dynamicMap (fromIntegral . imageWidth) dynamicImage
      height = dynamicMap (fromIntegral . imageHeight) dynamicImage
  case dynamicImage of
    -- Pixel formats convertable to RGB
    ImageRGB8   image -> texImage2dRgb8 srgb width height image
    ImageYCbCr8 image -> texImage2dRgb8 srgb width height
                           . convertImage $ image
    ImageCMYK8  image -> texImage2dRgb8 srgb width height
                           . convertImage $ image
    -- RGBA
    ImageRGBA8  image -> texImage2dRgba8 srgb width height image
    -- Anything else we can't load for now
    _                 -> error "loadTexture: unsupported pixel format"
  -- Mipmap generation
  when (isJust . snd $ minF) $ GL.generateMipmap' GL.Texture2D
  -- Unbind the texture
  GL.textureBinding GL.Texture2D $= Nothing
  return texture
 where
  toGlWrapMode :: TextureWrapMode -> (GL.Repetition, GL.Clamping) 
  toGlWrapMode mode = case mode of
    ClampToEdge     -> (GL.Repeated, GL.ClampToEdge)
    MirroredRepeat -> (GL.Mirrored, GL.Repeat)
    Repeat          -> (GL.Repeated, GL.Repeat)
--    Border {}   -> (GL.Repeated, GL.ClampToBorder)

texImage2dRgb8 :: Bool -> ImageWidth -> ImageHeight -> Image PixelRGB8 -> IO ()
texImage2dRgb8 srgb width height image = V.unsafeWith (imageData image) $ \ptr ->
  GL.texImage2D
    GL.Texture2D
    GL.NoProxy
    0
    (if srgb then GL.SRGB else GL.RGB')
    (GL.TextureSize2D width height)
    0
    (GL.PixelData GL.RGB GL.UnsignedByte (castPtr ptr))

texImage2dRgba8 :: Bool -> ImageWidth -> ImageHeight -> Image PixelRGBA8 -> IO ()
texImage2dRgba8 srgb width height image = V.unsafeWith (imageData image) $ \ptr ->
  GL.texImage2D
    GL.Texture2D
    GL.NoProxy
    0
    (if srgb then GL.SRGBAlpha else GL.RGBA')
    (GL.TextureSize2D width height)
    0
    (GL.PixelData GL.RGBA GL.UnsignedByte (castPtr ptr))

{-
  Missing support for the following image formats

  ImageY8 (Image Pixel8)	 A greyscale image.
  ImageY16 (Image Pixel16)	A greyscale image with 16bit components
  ImageY32 (Image Pixel32) A greyscale image with 32bit components
  ImageYF (Image PixelF)	 A greyscale HDR image
  ImageYA8 (Image PixelYA8)	 An image in greyscale with an alpha channel.
  ImageYA16 (Image PixelYA16)	 An image in greyscale with alpha channel on 16 bits.
  ImageRGB16 (Image PixelRGB16) An image in true color with 16bit depth.
  ImageRGBF (Image PixelRGBF)	 An image with HDR pixels
  ImageRGBA8 (Image PixelRGBA8)	 An image in true color and an alpha channel.
  ImageRGBA16 (Image PixelRGBA16)	 A true color image with alpha on 16 bits.
  ImageCMYK16 (Image PixelCMYK16)	An image in the colorspace CMYK and 16 bits precision
-}
