module Texture (
  loadTexture
) where

import Codec.Picture
import Codec.Picture.Types
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vector.Storable as V
import Foreign.Ptr

loadTexture :: IO GL.TextureObject
loadTexture = do
  eContainer <- readJpeg "assets/textures/container.jpg"
  let dynamicImage = either error id eContainer
      image = case dynamicImage of
        ImageRGB8 i-> convertImage i :: Image PixelRGB8
        ImageCMYK8 i-> convertImage i :: Image PixelRGB8
        ImageYCbCr8 i-> convertImage i :: Image PixelRGB8
        _ -> error "unable to read container texture"
      width = fromIntegral . imageWidth $ image
      height = fromIntegral . imageHeight $ image
  texture <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  V.unsafeWith (imageData image) $ \ptr ->
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 (GL.TextureSize2D width height) 0 (GL.PixelData GL.RGB GL.UnsignedByte (castPtr ptr))
  return texture
