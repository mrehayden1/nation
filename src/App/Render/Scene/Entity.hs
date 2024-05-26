module App.Render.Scene.Entity (
  entityCullingBounds,
  entityHasShadow,

  Models(..),
  entityModel,
  loadModels
) where

import qualified Data.Vector as V
import Linear
import Text.Printf

import App.Entity
import App.Entity.Collision3D
import App.Render.Model
import qualified App.Render.Model.GLTF.Material as Mat

entityCullingBounds :: Fractional a => Entity -> Maybe (Collision3D a)
entityCullingBounds =
  \case EntityOakTree -> Just $ CollisionSphere (V3 0 0 4.8692) 15
        _             -> Nothing

entityHasShadow :: Entity -> Bool
entityHasShadow =
  \case EntityPointer -> False
        _             -> True

data Models = Models {
  modelsCoin :: Model,
  modelsGrass :: Model,
  modelsOakTree :: Model,
  modelsPeasant :: Model,
  modelsPlayer :: Model,
  modelsPointer :: Model
}

entityModel :: Entity -> Models -> Model
entityModel entity Models{..} =
  case entity of
    EntityCoin -> modelsCoin
    EntityGrass -> modelsGrass
    EntityOakTree -> modelsOakTree
    EntityPeasant -> modelsPeasant
    EntityPlayer -> modelsPlayer
    EntityPointer -> modelsPointer

loadModels :: IO Models
loadModels = Models
  <$> loadCoin
  <*> loadGrass
  <*> loadOakTree
  <*> loadPeasant
  <*> loadPlayer
  <*> loadPointer

loadCoin :: IO Model
loadCoin = loadModel "assets/models/coin.glb"

loadOakTree :: IO Model
loadOakTree = loadModel "assets/models/oak1.glb"

loadPeasant :: IO Model
loadPeasant = loadModel "assets/models/peasant.glb"

loadPlayer :: IO Model
loadPlayer = loadModel "assets/models/horse.glb"

loadPointer :: IO Model
loadPointer = loadModel "assets/models/emerald.glb"

loadGrass :: IO Model
loadGrass = do
  let pathname = "assets/models/grass-tile.glb"
  printFileName pathname
  materials <- Mat.fromGlbFile pathname
  let indices' = V.fromList [0, 1, 2, 0, 2, 3]
      positions = V.fromList [
                    V3 (-50) 0 (-50),
                    V3 (-50) 0   50,
                    V3   50  0   50,
                    V3   50  0 (-50)]
      normals = V.fromList [V3 0 1 0, V3 0 1 0, V3 0 1 0, V3 0 1 0]
      tangents = V.fromList [V4 1 0 0 1, V4 1 0 0 1, V4 1 0 0 1, V4 1 0 0 1]
      texCoords = V.fromList . fmap (^/ textureScale) $ [
                    V2 (-50) (-50),
                    V2 (-50)   50 ,
                    V2   50    50 ,
                    V2   50  (-50)]
  prim <- meshPrimitive (materials V.! 0) Triangles indices' positions normals
            tangents texCoords mempty mempty
  let mesh = V.fromList [prim]
      node = Node mempty mempty mesh (Quaternion 1 0) 1 Nothing 0
  return $ Model (V.singleton node) (V.singleton 0) mempty 0
 where
  textureScale = 4.5

loadModel :: FilePath -> IO Model
loadModel pathname = do
  printFileName pathname
  fromGlbFile pathname

printFileName :: FilePath -> IO ()
printFileName = printf "Loading model \"%s\"...\n"
