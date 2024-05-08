module Main (
  main
) where

import Linear
import Graphics.Gloss
import Text.Printf

import App.Geometry
import App.Map

height, width :: Num a => a
height = 1080
width  = 1920

main :: IO ()
main = displayMapGeometry

displayMapGeometry :: IO ()
displayMapGeometry = do
  --seed <- randomIO
  -- Some seeds that terminate
  let seed = -662982938059047685
  --let seed = 2108559135846489037
  --let seed = -1967577792088230408
  --    MapData{..} = generateMapGeometry seed
      MapData{..} = generateTestMapGeometry seed
      MapGraph{..} = mapGraph
      picture =
        -- Make co-ordinate system match our game world (right-handed, +y = up)
        Scale 1 (-1)
          . Pictures $ [
              -- Points
              Pictures . fmap mkPointText $ mapGraphNodes,
              Pictures . fmap (mkPoint 10) $ mapGraphNodes,
              -- Edges
              --Pictures . fmap (uncurry mkLine) $ mapGraphEdges,
              -- Poisson discs
              --Pictures . fmap (mkDisc poiDiscRadius) $ mapGraphNodes,
              -- Room generatable area
              --Pictures . fmap (mkDisc (poiDiscRadius / 2)) $ mapGraphNodes,
              -- Grid lines
              --Pictures . fmap (uncurry mkLine) $ gridLines,
              -- Shapes
              --Pictures . fmap drawPolygon $ mapRoomGeometry,
              -- Paths
              --Pictures . fmap drawPolygon $ mapPathGeometry
              -- Mesh
              drawPolygon mapMesh,
              -- Trees
              Pictures . fmap (mkPoint 50 . mapTreePosition) $ mapTrees
            ]
  --printf "Seed: %d\n" seed
  display
    (InWindow
       "Map generation visualiser" -- window title
       (width, height)  -- window size
       (10, 10))        -- window position
    (light black)       -- background color
    picture
 where
  gridLines = [
      (V2  (1/6) (-1/2), V2  (1/6)  (1/2)),
      (V2 (-1/6) (-1/2), V2 (-1/6)  (1/2)),
      (V2 (-1/2)  (1/6), V2  (1/2)  (1/6)),
      (V2 (-1/2) (-1/6), V2  (1/2) (-1/6))
    ]

drawPolygon :: Polygon Float -> Picture
drawPolygon p =
  Pictures [
    Pictures . fmap mkShape . polygonFaces $ p,
    Pictures . fmap (Pictures . fmap (mkPoint 10)) . polygonFaces $ p,
    Pictures . fmap mkShape . polygonHoles $ p,
    Pictures . fmap (Pictures . fmap (mkPoint 10)) . polygonHoles $ p
  ]

mkLine :: V2 Float -> V2 Float -> Picture
mkLine x y = Color (dark white) . Line . fmap (vec2ToTuple . (* height))
               $ [x, y]

mkPointText :: V2 Float -> Picture
mkPointText (V2 x y) = Translate (x * height) (y * height) . Color (dark white)
  . Text . printf "%.10f, %.10f" x $ y

mkPoint :: Float -> V2 Float -> Picture
mkPoint sz = Color (dark white) . ($ ThickCircle sz (sz * 2))
  . uncurry Translate . vec2ToTuple . (* height)

mkDisc :: Float -> V2 Float -> Picture
mkDisc r = Color (dark white) . ($ Circle (r * height))
  . uncurry Translate . vec2ToTuple . (* height)

mkShape :: [V2 Float] -> Picture
mkShape vs = Pictures . fmap (uncurry mkLine) . zip vs . drop 1 . cycle $ vs

mkPolyline :: [V2 Float] -> Picture
mkPolyline ps = Pictures . fmap (uncurry mkLine) . zip ps . drop 1 $ ps

vec2ToTuple :: V2 a -> (a, a)
vec2ToTuple (V2 x y) = (x, y)
