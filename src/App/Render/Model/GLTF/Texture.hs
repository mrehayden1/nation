module App.Render.Model.GLTF.Texture (
  loadTexture
) where

import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Text.GLTF.Loader as G

import qualified App.Render.Texture as T

loadTexture :: Vector G.Image
  -> Vector G.Sampler
  -> T.SRGB
  -> G.Texture
  -> IO GL.TextureObject
loadTexture images samplers srgb G.Texture{..} = do
  let mSampler = fmap (samplers !) textureSamplerId
      wrapS = maybe T.Repeat (toWrapMode . G.samplerWrapS) mSampler
      wrapT = maybe T.Repeat (toWrapMode . G.samplerWrapS) mSampler
      minF = maybe (T.Nearest, Nothing) toMinFilter
               $ G.samplerMinFilter =<< mSampler
      magF = maybe T.Nearest toMagFilter
               $ G.samplerMagFilter =<< mSampler
  let image = fromMaybe T.missingImageData
                $ G.imageData . (images !) =<< textureSourceId
  T.decodeImage srgb minF magF wrapS wrapT image
 where
  toMinFilter :: G.MinFilter -> T.MinificationFilter
  toMinFilter f = case f of
    G.MinNearest              -> (T.Nearest, Nothing)
    G.MinLinear               -> (T.Linear', Nothing)
    G.MinNearestMipmapNearest -> (T.Nearest, Just T.Nearest)
    G.MinLinearMipmapNearest  -> (T.Linear', Just T.Nearest)
    G.MinNearestMipmapLinear  -> (T.Nearest, Just T.Linear')
    G.MinLinearMipmapLinear   -> (T.Linear', Just T.Linear')

  toMagFilter :: G.MagFilter -> T.MagnificationFilter
  toMagFilter f = case f of
    G.MagLinear  -> T.Linear'
    G.MagNearest -> T.Nearest

  toWrapMode :: G.SamplerWrap -> T.TextureWrapMode
  toWrapMode w = case w of
    G.ClampToEdge    -> T.ClampToEdge
    G.MirroredRepeat -> T.MirroredRepeat
    G.Repeat         -> T.Repeat
