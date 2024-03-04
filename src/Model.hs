module Model (
  loadGrass
) where

import Data.Tree
import Data.Vector as V
import Linear
import Text.Printf

import Render.Model
import Render.Model.GLTF.Material as Mat

loadGrass :: IO Model
loadGrass = do
  let pathname = "assets/models/grass-tile.glb"
  printf "Loading model \"%s\"...\n" pathname
  materials <- Mat.fromGlbFile pathname
  let indices = V.fromList [0, 1, 2, 0, 2, 3]
      positions = V.fromList [V3 (-50) 0 (-50),
                              V3 (-50) 0   50,
                              V3   50  0   50,
                              V3   50  0 (-50)]
      normals = V.fromList [V3 0 1 0, V3 0 1 0, V3 0 1 0, V3 0 1 0]
      tangents = V.fromList [V4 1 0 0 1, V4 1 0 0 1, V4 1 0 0 1, V4 1 0 0 1]
      texCoords = V.fromList [V2 (-50/4.5) (-50/4.5),
                              V2 (-50/4.5) ( 50/4.5),
                              V2 ( 50/4.5) ( 50/4.5),
                              V2 ( 50/4.5) (-50/4.5)]
  prim <- meshPrimitive (materials ! 0) Triangles indices positions normals
            tangents texCoords
  let mesh = V.fromList [prim]
      node = SceneNode mempty (Just mesh) (Quaternion 1 0) 1 0
  return . Model . Node node $ []
