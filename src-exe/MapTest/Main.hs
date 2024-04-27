module Main (
  main
) where

import Linear
import Graphics.Gloss
import System.Random
import Text.Printf

import App.Map as M

height, width :: Num a => a
height = 1080
width  = 1920

main :: IO ()
main = displayMapGeometry

displayPath :: IO ()
displayPath = do
  let seed = -1967577792088230408
      (route, path) = flip evalRand (mkStdGen seed) . perlinPath p0 $ p1
  display
    (InWindow
       "Map generation visualiser" -- window title
       (width, height)   -- window size
       (10, 10))         -- window position
    (light black)        -- background color
    (picture route path) -- picture to display
 where
  picture :: [V2 Float] -> [V2 Float] -> Picture
  picture route path =
    Pictures [
      mkPolyline route,
      Pictures . fmap (mkPoint 0.1) $ path,
      Pictures . fmap mkPointText $ [p0, p1],
      Pictures . fmap (mkPoint 0.1) $ [p0, p1]
    ]

  p0 = 0
  p1 = V2 0.1537222900 (-0.0081150530)

displayMapGeometry :: IO ()
displayMapGeometry = do
  --seed <- randomIO
  --let seed = -662982938059047685
  --let seed = 2108559135846489037
  let seed = -1967577792088230408
  --printf "Seed: %d\n" seed
  let ((ps, es), shapes, paths) = generateMapGeometry seed
  display
    (InWindow
       "Map generation visualiser" -- window title
       (width, height)  -- window size
       (10, 10))        -- window position
    (light black)       -- background color
    (picture ps es shapes paths) -- picture to display
 where
  picture :: [V2 Float]
    -> [(V2 Float, V2 Float)]
    -> [[V2 Float]]
    -> [M.Path]
    -> Picture
  picture points edges shapes paths =
    -- Make co-ordinate system match our game world, right-handed, north-east =
    -- (+x, -y)
    --Scale 1 (-1)
    Scale 1 1
      . Pictures $ [
        -- Points
        --Pictures . fmap mkPoint 4 $ points,
        Pictures . fmap mkPointText $ points,
        -- Edges
        --Pictures . fmap (uncurry mkLine) $ edges,
        -- Poisson discs
        --Pictures . fmap (mkDisc discRadius) $ points,
        -- Room generatable area
        --Pictures . fmap (mkDisc (discRadius / 2)) $ points,
        -- Grid lines
        --Pictures . fmap (uncurry mkLine) $ gridLines,
        -- Shapes
        Pictures . fmap mkShape $ shapes,
        Pictures . fmap (Pictures . fmap (mkPoint 0.1)) $ shapes,
        -- Paths
        Pictures . fmap (mkShape . snd) $ paths,
        Pictures . fmap (Pictures . fmap (mkPoint 0.1) . snd) $ paths
      ]

  gridLines = [
      (V2  (1/6) (-1/2), V2  (1/6)  (1/2)),
      (V2 (-1/6) (-1/2), V2 (-1/6)  (1/2)),
      (V2 (-1/2)  (1/6), V2  (1/2)  (1/6)),
      (V2 (-1/2) (-1/6), V2  (1/2) (-1/6))
    ]

mkLine :: V2 Float -> V2 Float -> Picture
mkLine x y = Color white . Line . fmap (vec2ToTuple . (* height)) $ [x, y]

mkPointText :: V2 Float -> Picture
mkPointText (V2 x y) = Translate (x * height) (y * height) . Color white
  . Scale 0.01 0.01 . Text . printf "%.10f, %.10f" x $ y

mkPoint :: Float -> V2 Float -> Picture
mkPoint sz = Color white . ($ ThickCircle sz (sz * 2))
  . uncurry Translate . vec2ToTuple . (* height)

mkDisc :: Float -> V2 Float -> Picture
mkDisc r = Color white . ($ Circle (r * height))
  . uncurry Translate . vec2ToTuple . (* height)

mkShape :: [V2 Float] -> Picture
mkShape vs = Pictures . fmap (uncurry mkLine) . zip vs . drop 1 . cycle $ vs

mkPolyline :: [V2 Float] -> Picture
mkPolyline ps = Pictures . fmap (uncurry mkLine) . zip ps . drop 1 $ ps

vec2ToTuple :: V2 a -> (a, a)
vec2ToTuple (V2 x y) = (x, y)
