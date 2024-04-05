module Entity (
  Entities(..),

  CoinE(coinECollision, coinEModel),
  GrassE(grassEModel),
  PlayerE(playerECollision, playerEModel),
  PeasantE(peasantECollision, peasantEModel),
  PointerE(pointerEModel),

  Collision,
  Model,

  loadEntities
) where

import Data.Vector as V hiding (mapM)
import Linear
import Text.Printf

import Entity.Collision
import Render.Model as Model
import Render.Model.GLTF.Material as Mat

data Entities = Entities {
  entitiesCoin :: CoinE,
  entitiesGrass :: GrassE,
  entitiesPeasant :: PeasantE,
  entitiesPlayer :: PlayerE,
  entitiesPointer :: PointerE
}

data CoinE = CoinE {
  coinECollision :: Collision,
  coinEModel :: Model
}

newtype GrassE = GrassE {
  grassEModel :: Model
}

data PeasantE = PeasantE {
  peasantECollision :: Collision,
  peasantEModel :: Model
}

data PlayerE = PlayerE {
  playerECollision :: Collision,
  playerEModel :: Model
}

newtype PointerE = PointerE {
  pointerEModel :: Model
}

loadEntities :: IO Entities
loadEntities = Entities
    <$> loadCoin
    <*> loadGrass
    <*> loadPeasant
    <*> loadPlayer
    <*> (PointerE <$> loadModel "assets/models/emerald.glb")

loadCoin :: IO CoinE
loadCoin = do
  model <- loadModel "assets/models/coin.glb"
  -- Circle around the model origin.
  let collision = CollisionCircle 0 0.62
  return . CoinE collision $ model

loadPeasant :: IO PeasantE
loadPeasant = do
  model <- loadModel "assets/models/peasant.glb"
  let collision = CollisionPolygon [
          V2 (-6) (-3),
          V2   6  (-3),
          V2   6    3 ,
          V2 (-6)   3
        ]
  return . PeasantE collision $ model

loadPlayer :: IO PlayerE
loadPlayer = do
  model <- loadModel "assets/models/horse.glb"
  let collision = CollisionPolygon [
          V2 (-1)   (-0.33),
          V2 (-1)     0.33 ,
          V2   1.61   0.33 ,
          V2   1.61 (-0.33)
        ]
  return . PlayerE collision $ model

loadGrass :: IO GrassE
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
      model = Model (V.singleton node) (V.singleton 0) mempty 0
  return . GrassE $ model
 where
  textureScale = 4.5

loadModel :: FilePath -> IO Model
loadModel pathname = do
  printFileName pathname
  Model.fromGlbFile pathname

printFileName :: FilePath -> IO ()
printFileName = printf "Loading model \"%s\"...\n"
