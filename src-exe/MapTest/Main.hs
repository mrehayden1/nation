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
              Pictures . fmap drawPointText $ mapGraphNodes,
              Pictures . fmap (drawPoint 10) $ mapGraphNodes,
              -- Edges
              --Pictures . fmap (uncurry drawLine) $ mapGraphEdges,
              -- Poisson discs
              --Pictures . fmap (drawDisc poiDiscRadius) $ mapGraphNodes,
              -- Room generatable area
              --Pictures . fmap (drawDisc (poiDiscRadius / 2)) $ mapGraphNodes,
              -- Grid lines
              --Pictures . fmap (uncurry drawLine) $ gridLines,
              -- Shapes
              --Pictures . fmap drawPolygon $ mapRoomGeometry,
              -- Paths
              --Pictures . fmap drawPolygon $ mapPathGeometry
              -- Mesh
              drawTristrip mapMesh,
              -- Trees
              Pictures . fmap (drawPoint 50 . mapTreePosition) $ mapTrees
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
    Pictures . fmap drawShape . polygonFaces $ p,
    Pictures . fmap (Pictures . fmap (drawPoint 10)) . polygonFaces $ p,
    Pictures . fmap drawShape . polygonHoles $ p,
    Pictures . fmap (Pictures . fmap (drawPoint 10)) . polygonHoles $ p
  ]

drawTristrip :: Tristrip Float -> Picture
drawTristrip = Pictures . fmap drawStrip . unTristrip
 where
  drawStrip t =
    Pictures . zipWith3 drawTriangle t (drop 1 t) $ (drop 2 t)

drawLine :: V2 Float -> V2 Float -> Picture
drawLine x y = Color (dark white) . Line . fmap (vec2ToTuple . (* height))
               $ [x, y]

drawPointText :: V2 Float -> Picture
drawPointText (V2 x y) = Translate (x * height) (y * height) . Color (dark white)
  . Text . printf "%.10f, %.10f" x $ y

drawPoint :: Float -> V2 Float -> Picture
drawPoint sz = Color (dark white) . ($ ThickCircle sz (sz * 2))
  . uncurry Translate . vec2ToTuple . (* height)

drawDisc :: Float -> V2 Float -> Picture
drawDisc r = Color (dark white) . ($ Circle (r * height))
  . uncurry Translate . vec2ToTuple . (* height)

drawShape :: [V2 Float] -> Picture
drawShape vs = Pictures . fmap (uncurry drawLine) . zip vs . drop 1 . cycle $ vs

drawPolyline :: [V2 Float] -> Picture
drawPolyline ps = Pictures . fmap (uncurry drawLine) . zip ps . drop 1 $ ps

drawTriangle :: V2 Float -> V2 Float -> V2 Float -> Picture
drawTriangle a b c = drawPolyline [a, b, c, a]

vec2ToTuple :: V2 a -> (a, a)
vec2ToTuple (V2 x y) = (x, y)
