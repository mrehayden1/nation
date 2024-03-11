module Model (
  ModelName(..),
  loadModels
) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Tree
import Data.Vector ((!))
import qualified Data.Vector as V
import Linear
import Text.Printf

import Render.Model as Model
import Render.Model.GLTF.Material as Mat

data ModelName =
    Fauna
  | Grass
  | Horse
  | Monument
  | Pointer
 deriving (Eq, Ord)

loadModels :: IO (Map ModelName Model)
loadModels = fmap M.fromList . mapM (uncurry (fmap . (,))) $ [
    (Fauna, loadModel "assets/models/fauna.glb"),
    (Grass, loadGrass),
    (Horse, loadModel "assets/models/horse.glb"),
    (Pointer, loadModel "assets/models/emerald.glb"),
    (Monument, loadModel "assets/models/monument.glb")
  ]

loadGrass :: IO Model
loadGrass = do
  let pathname = "assets/models/grass-tile.glb"
  printFileName pathname
  materials <- Mat.fromGlbFile pathname
  let indices = V.fromList [0, 1, 2, 0, 2, 3]
      positions = V.fromList [V3 (-50) 0 (-50),
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
  prim <- meshPrimitive (materials ! 0) Triangles indices positions normals
            tangents texCoords
  let mesh = V.fromList [prim]
      node = SceneNode mempty (Just mesh) (Quaternion 1 0) 1 0
  return . Model . Node node $ []
 where
  textureScale = 4.5

loadModel :: FilePath -> IO Model
loadModel pathname = do
  printFileName pathname
  Model.fromGlbFile pathname

printFileName :: FilePath -> IO ()
printFileName = printf "Loading model \"%s\"...\n"
