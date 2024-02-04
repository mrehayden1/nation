module Render.Texture (
  TextureWrapMode(..),
  GL.TextureFilter(..),
  loadTexture
) where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Data.StateVar
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import GHC.Int
import qualified Graphics.Rendering.OpenGL as GL

type ImageWidth = Int32
type ImageHeight = Int32

data TextureWrapMode = ClampToEdge | Repeat | Mirror | Border Float Float Float Float

-- Loads a 2D texture from an image file
loadTexture :: FilePath
  -> GL.MinificationFilter
  -> GL.MagnificationFilter
  -> TextureWrapMode
  -> IO GL.TextureObject
loadTexture filePath minFilter magFilter wrapMode = do
  eImage <- readImage filePath
  let dynamicImage = either error id eImage
      width = fromIntegral . dynamicMap imageWidth $ dynamicImage
      height = fromIntegral . dynamicMap imageHeight $ dynamicImage
  -- Create the texture object
  texture <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= (minFilter, magFilter)
  case wrapMode of
    Border r g b a -> GL.textureBorderColor GL.Texture2D $= GL.Color4 r g b a
    _              -> return ()
  let wrapMode' = case wrapMode of
        ClampToEdge -> (GL.Repeated, GL.ClampToEdge)
        Repeat      -> (GL.Repeated, GL.Repeat)
        Mirror      -> (GL.Mirrored, GL.Repeat)
        Border {}   -> (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.S $= wrapMode'
  case dynamicImage of
    -- Pixel formats converted to RGB
    ImageRGB8   i -> loadRGB8 width height . flipVertically $ i
    ImageYCbCr8 i -> loadRGB8 width height . flipVertically . convertImage $ i
    ImageCMYK8  i -> loadRGB8 width height . flipVertically . convertImage $ i
    -- RGBA
    ImageRGBA8  i -> loadRGBA8 width height . flipVertically $ i
    -- Anything else we can't load for now
    _             -> error "loadTexture: unsupported pixel format"
  -- Unbind the texture
  GL.textureBinding GL.Texture2D $= Nothing
  return texture
 where
  loadRGB8 :: ImageWidth -> ImageHeight -> Image PixelRGB8 -> IO ()
  loadRGB8 width height image = V.unsafeWith (imageData image) $ \ptr ->
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' (GL.TextureSize2D width height) 0 (GL.PixelData GL.RGB GL.UnsignedByte (castPtr ptr))

  loadRGBA8 :: ImageWidth -> ImageHeight -> Image PixelRGBA8 -> IO ()
  loadRGBA8 width height image = V.unsafeWith (imageData image) $ \ptr ->
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA' (GL.TextureSize2D width height) 0 (GL.PixelData GL.RGBA GL.UnsignedByte (castPtr ptr))

{-
  Missing support for the following JuicyPixel image formats

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
