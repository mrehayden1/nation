module App.Render.Model.GLTF.Material (
  fromGlbFile,

  adaptMaterial
) where

import Control.Monad
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Text.GLTF.Loader as G
import Text.GLTF.Loader (Gltf(..))
import Text.Printf

import App.Render.Model.GLTF.Texture
import App.Render.Model.Model

fromGlbFile :: FilePath -> IO (Vector Material)
fromGlbFile pathname = do
  printf "Loading materials from model \"%s\"...\n" pathname
  eGlb <- G.fromBinaryFile pathname
  gltf <- case eGlb of
    Left  _ -> fail "fromGlbFile: Failed to read GLB."
    Right g -> return $ G.unGltf g
  let Gltf{..} = gltf
      baseColorTextureIxs = V.catMaybes . fmap getMaterialBaseColorTextureId
        $ gltfMaterials
  textures <- mapM (uncurry $ loadTexture gltfImages gltfSamplers)
                . V.imap (\i t -> (i `elem` baseColorTextureIxs, t))
                $ gltfTextures
  return . fmap (adaptMaterial textures) $ gltfMaterials
 where
  getMaterialBaseColorTextureId = pure . G.textureId <=< G.pbrBaseColorTexture <=< G.materialPbrMetallicRoughness

adaptMaterial :: Vector GL.TextureObject -> G.Material -> Material
adaptMaterial textures G.Material{..} =
  let colorFactor = maybe defaultBaseColorFactor G.pbrBaseColorFactor
                      materialPbrMetallicRoughness
      colorTexture =
        fmap ((textures !) . G.textureId)
          $ G.pbrBaseColorTexture =<< materialPbrMetallicRoughness
      normalMap = fmap ((textures !) . G.normalTextureId) materialNormalTexture
      normalMapScale = maybe 1 G.normalTextureScale materialNormalTexture
      metallicRoughnessTexture =
        fmap ((textures !) . G.textureId)
          $ G.pbrMetallicRoughnessTexture =<< materialPbrMetallicRoughness
      metallicFactor = maybe defaultMetallicFactor G.pbrMetallicFactor
                         materialPbrMetallicRoughness
      roughnessFactor = maybe defaultRoughnessFactor G.pbrRoughnessFactor
                         materialPbrMetallicRoughness
  in Material {
       materialAlphaCutoff = materialAlphaCutoff,
       materialAlphaMode = materialAlphaMode,
       materialBaseColorFactor = colorFactor,
       materialBaseColorTexture = colorTexture,
       materialDoubleSided = materialDoubleSided,
       materialMetallicFactor = metallicFactor,
       materialMetallicRoughnessTexture = metallicRoughnessTexture,
       materialNormalTexture = normalMap,
       materialNormalTextureScale = normalMapScale,
       materialRoughnessFactor = roughnessFactor
    }
