module Entity (
  Entities(..),

  Coin(coinCollision, coinModel),
  Grass(grassModel),
  Player(playerCollision, playerModel),
  Pointer(pointerModel),

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
  entitiesCoin :: Coin,
  entitiesGrass :: Grass,
  entitiesPlayer :: Player,
  entitiesPointer :: Pointer
}

data Coin = Coin {
  coinCollision :: Collision,
  coinModel :: Model
}

newtype Grass = Grass {
  grassModel :: Model
}

data Player = Player {
  playerCollision :: Collision,
  playerModel :: Model
}

newtype Pointer = Pointer {
  pointerModel :: Model
}

loadEntities :: IO Entities
loadEntities = Entities
    <$> loadCoin
    <*> loadGrass
    <*> loadPlayer
    <*> (Pointer <$> loadModel "emerald" "assets/models/emerald.glb")
 where
  loadCoin = do
    model <- loadModel "coin" "assets/models/coin.glb"
    -- Circle around the model origin.
    let collision = CollisionCircle 0 0.62
    return . Coin collision $ model

  loadPlayer = do
    model <- loadModel "horse" "assets/models/horse.glb"
    let collision = CollisionPolygon [
            V2 (-1)   (-0.33),
            V2 (-1)     0.33 ,
            V2   1.61   0.33 ,
            V2   1.61 (-0.33)
          ]
    return . Player collision $ model

loadGrass :: IO Grass
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
      model = Model "grass" (V.singleton node) (V.singleton 0) mempty 0
  return . Grass $ model
 where
  textureScale = 4.5

loadModel :: String -> FilePath -> IO Model
loadModel name pathname = do
  printFileName pathname
  Model.fromGlbFile name pathname

printFileName :: FilePath -> IO ()
printFileName = printf "Loading model \"%s\"...\n"
