module Model (
  Model,
  ModelName(..),
  Models,

  loadModels,
  getModel
) where

import Data.Map as M
import Data.Maybe
import Data.Vector as V hiding (mapM)
import Linear
import Text.Printf

import Render.Model as Model
import Render.Model.GLTF.Material as Mat

data ModelName =
    Coin
  | Cube
  | Grass
  | Horse
  | Pointer
 deriving (Eq, Ord, Show)

newtype Models = Models { unModels :: Map ModelName Model }

loadModels :: IO Models
loadModels = fmap (Models . M.fromList) . mapM (uncurry (fmap . (,))) $ [
    (Cube, loadModel "cube" "assets/models/animation-debug-cube.glb"),
    (Coin, loadModel "coin" "assets/models/coin.glb"),
    (Grass, loadGrass),
    (Horse, loadModel "horse" "assets/models/horse.glb"),
    (Pointer, loadModel "emerald" "assets/models/emerald.glb")
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
  prim <- meshPrimitive (materials V.! 0) Triangles indices positions normals
            tangents texCoords mempty mempty
  let mesh = V.fromList [prim]
      node = Node mempty mempty False (Just mesh) (Quaternion 1 0) 1 Nothing 0
  return . Model "grass" (V.singleton node) (V.singleton 0) mempty $ 0
 where
  textureScale = 4.5

loadModel :: String -> FilePath -> IO Model
loadModel name pathname = do
  printFileName pathname
  Model.fromGlbFile name pathname

printFileName :: FilePath -> IO ()
printFileName = printf "Loading model \"%s\"...\n"

getModel :: Models -> ModelName -> Model
getModel models name =
  fromMaybe (error . printf "%s model not found" . show $ name)
    . (unModels models M.!?) $ name
