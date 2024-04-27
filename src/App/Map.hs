module App.Map (
  module Control.Monad.Random,

  Path,
  generateMapGeometry,
  generateMapGraph,
  discRadius,

  perlinPath
) where

import Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import Algorithms.Geometry.DelaunayTriangulation.Types
import Control.Applicative
import Control.Lens
import Control.Monad.Random
import Data.Bifunctor
import Data.Ext
import Data.Function
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.RealNumber.Rational
import Geometry.Point
import qualified Geometry.Vector.VectorFamily as VF
import Linear
import Numeric.Noise.Perlin

data MapGrid = Center | North | NorthEast | East | SouthEast | South
                 | SouthWest | West | NorthWest
 deriving (Eq)

type Graph = ([V2 Float], [(V2 Float, V2 Float)])
type Room = [V2 Float]
type Path = ([V2 Float], [V2 Float]) -- Route + path geometry

type MapData = (Graph, [Room], [Path])

discRadius :: Fractional a => a
discRadius = 0.15

generateMapGeometry :: Int -> MapData
generateMapGeometry seed = flip evalRand (mkStdGen seed) $ do
  graph@(points, edges) <- generateMapGraph
  rooms <- mapM perlinLoop points
  paths <- mapM (uncurry perlinPath) edges
  return (graph, rooms, paths)

perlinPath :: RandomGen g => V2 Float -> V2 Float -> Rand g Path
perlinPath p0 p1 = do
  route <- perlinRoute p0 p1
  let pairs = zip route . drop 1 $ route
      norms = fmap (uncurry $ (((* 0.005) . vecNormal) .) . (-)) pairs
      path' = (++) (zipWith (+) route norms)
                . reverse . zipWith (-) route $ norms
  return (route, path')

-- Make a route between two points by sampling a perlin distribution on the
-- circumfrence of a circle
perlinRoute :: RandomGen g => V2 Float -> V2 Float -> Rand g [V2 Float]
perlinRoute p0 p1 = do
  seed <- getRandom
  let dist = perlin seed 1 0.5 0.5 -- seed, octaves, scale, persistence
      vec = p1 - p0
      norm' = vecNormal vec
  return . fmap (point' dist vec norm') $ [0..n]
 where
  point' :: Perlin -> V2 Float -> V2 Float -> Int -> V2 Float
  point' dist vec norm' n' =
    let d = realToFrac n'/ realToFrac n
        t = d * 2 * pi
        -- attenuation factor increasing towards ends of paths so they start
        -- and end at the given points
        atten = sin $ d * pi
    in (+ (vec ^* d)) . (+ p0) . (*^ norm') . (* wiggle) . (* atten)
         . circularPerlin dist $ t

  n :: Int
  n = 100

  wiggle = 0.025

-- Make a comlpete loop by sampling a perlin distribution on the circumfrence
-- of a circle
perlinLoop :: RandomGen g => V2 Float -> Rand g [V2 Float]
perlinLoop c = do
  seed <- getRandom
  let dist = perlin seed 1 0.5 0.5 -- seed, octaves, scale, persistence
  return . fmap (point' dist) $ [0..(n - 1)]
 where
  point' :: Perlin -> Int -> V2 Float
  point' p n' =
    let t = (/ realToFrac n) . ((2 * pi) *) . realToFrac $ n'
    in (+ c) . (*^ circlePoint t) . (* 0.06) . (+ 0.5) . (/ 2)
         . circularPerlin p $ t

  n :: Int
  n = 100

  -- The point on the unit circle at theta
  circlePoint :: (Floating a) => a -> V2 a
  circlePoint = liftA2 V2 cos sin

-- Sample a perlin distribution on the circumfrence of the unit cirlce at
-- theta
circularPerlin :: (Floating a, Real a) => Perlin -> a -> a
circularPerlin p = realToFrac . (/ 2) . (+ 1)
  . noiseValue p . liftA2 (0,,) (realToFrac . cos) (realToFrac . sin)

generateMapGraph :: RandomGen g => Rand g ([V2 Float], [(V2 Float, V2 Float)])
generateMapGraph = do
  (ps :: [V2 Float :+ MapGrid]) <- generatePointsOfInterest
  let edges = validEdges . triangulate $ ps
  return (fmap _core ps, fmap (bimap _core _core) edges)
 where
  validEdges :: forall a. (Floating a, Ord a)
    => [(V2 a :+ MapGrid, V2 a :+ MapGrid)]
    -> [(V2 a :+ MapGrid, V2 a :+ MapGrid)]
  validEdges edges = filter validEdge edges
   where
    shortest g h = minimumBy (compare `on` edgeDistance)
      . filter (between g h) $ edges

    validEdge :: (V2 a :+ MapGrid, V2 a :+ MapGrid) -> Bool
    validEdge =
      fmap and . sequence $ [
        -- If starting and ending in the center only allow edges to and from
        -- the origin
        liftA2 implies (between Center Center) fromOrigin,
        -- Only adjacent grid squares can be linked
        fmap or . sequence $ [
          toSelf,
          between Center North,
          between Center East,
          between Center South,
          between Center West,
          between North NorthEast,
          between East NorthEast,
          between East SouthEast,
          between South SouthEast,
          between South SouthWest,
          between West SouthWest,
          between West NorthWest,
          between North NorthWest
        ],
        -- Only allow the shortest path between the Center and adjancent grid
        -- squares
        liftA2 implies (between Center North) (== shortest Center North),
        liftA2 implies (between Center East) (== shortest Center East),
        liftA2 implies (between Center South) (== shortest Center South),
        liftA2 implies (between Center West) (== shortest Center West),
        liftA2 implies (between North NorthWest) (== shortest North NorthWest),
        liftA2 implies (between North NorthEast) (== shortest North NorthEast),
        liftA2 implies (between East NorthEast) (== shortest East NorthEast),
        liftA2 implies (between East SouthEast) (== shortest East SouthEast),
        liftA2 implies (between South SouthEast) (== shortest South SouthEast),
        liftA2 implies (between South SouthWest) (== shortest South SouthWest),
        liftA2 implies (between West SouthWest) (== shortest West SouthWest),
        liftA2 implies (between West NorthWest) (== shortest West NorthWest)
      ]

  fromOrigin :: (Eq a, Num a) => (V2 a :+ MapGrid, V2 a :+ MapGrid) -> Bool
  fromOrigin = liftA2 (||) ((== 0) . _core . fst) ((== 0) . _core . snd)

  fromTo :: MapGrid -> MapGrid -> (V2 a :+ MapGrid, V2 a :+ MapGrid) -> Bool
  fromTo g h = liftA2 (&&) ((== g) . _extra . fst) ((== h) . _extra . snd)

  between :: MapGrid -> MapGrid -> (V2 a :+ MapGrid, V2 a :+ MapGrid) -> Bool
  between g h = liftA2 (||) (fromTo g h) (fromTo h g)

  toSelf :: (V2 a :+ MapGrid, V2 a :+ MapGrid) -> Bool
  toSelf = liftA2 (==) (_extra . fst) (_extra . snd)

  edgeDistance :: Floating a => (V2 a :+ MapGrid, V2 a :+ MapGrid) -> a
  edgeDistance = liftA2 distance (_core . snd) (_core . fst)

generatePointsOfInterest :: forall g a. (RandomGen g, Floating a, Ord a, Random a)
  => Rand g [V2 a :+ MapGrid]
generatePointsOfInterest = do
  c  <- addPoints' [0] 5 (-1/6)  (1/6) (-1/6)  (1/6) []
  n  <- addPoints      4 (-1/6)  (1/6) (-1/2) (-1/6) c
  ne <- addPoints      3  (1/6)  (1/2) (-1/2) (-1/6) $ c <> n
  e  <- addPoints      4  (1/6)  (1/2) (-1/6)  (1/6) $ c <> n <> ne
  se <- addPoints      3  (1/6)  (1/2)  (1/6)  (1/2) $ c <> n <> ne <> e
  s  <- addPoints      4 (-1/6)  (1/6)  (1/6)  (1/2) $ c <> n <> ne <> e <> se
  sw <- addPoints      3 (-1/2) (-1/6)  (1/6)  (1/2) $ c <> n <> ne <> e <> se <> s
  w  <- addPoints      4 (-1/2) (-1/6) (-1/6)  (1/6) $ c <> n <> ne <> e <> se <> s <> sw
  nw <- addPoints      3 (-1/2) (-1/6) (-1/2) (-1/6) $ c <> n <> ne <> e <> se <> s <> sw <> w
  return $ fmap (:+ Center) c
    <> fmap (:+ North) n
    <> fmap (:+ NorthEast) ne
    <> fmap (:+ East) e
    <> fmap (:+ SouthEast) se
    <> fmap (:+ South) s
    <> fmap (:+ SouthWest) sw
    <> fmap (:+ West)  w
    <> fmap (:+ NorthWest)  nw
 where
  addPoints :: Int
    -> a
    -> a
    -> a
    -> a
    -> [V2 a]
    -> Rand g [V2 a]
  addPoints = addPoints' []

  addPoints' :: [V2 a]
    -> Int
    -> a
    -> a
    -> a
    -> a
    -> [V2 a]
    -> Rand g [V2 a]
  addPoints' ps' n xMin xMax yMin yMax ps = do
    if length ps' >= n
      then return ps'
      else do
        rs <- fmap (take 100000) getRandoms
        let p' = head . filter valid . fmap (subtract 0.5) $ rs
        addPoints' (p' : ps') n xMin xMax yMin yMax ps
   where
    valid :: V2 a -> Bool
    valid = liftA2 (&&)
              inBox
              (not . flip any (ps <> ps') . flip inDisc discRadius)

    inBox :: V2 a -> Bool
    inBox = liftA2 (&&) (inBox' xMin xMax _x) (inBox' yMin yMax _y)
     where
      inBox' :: a
        -> a
        -> Lens' (V2 a) a
        -> V2 a
        -> Bool
      inBox' mn mx f = liftA2 (&&) ((>= mn) . view f) ((<= mx) . view f)

    inDisc :: V2 a -> a -> V2 a -> Bool
    inDisc c r p = r >= distance p c

triangulate :: forall d. [V2 Float :+ d] -> [(V2 Float :+ d, V2 Float :+ d)]
triangulate ps =
  let ps' = fmap vec2FloatToPoint2Real . NE.fromList $ ps
               :: NonEmpty (Point 2 (RealNumber 6) :+ d)
      t = delaunayTriangulation ps'
      edges = fmap (bimap point2RealToVec2Float point2RealToVec2Float)
                . edgesAsPoints $ t
  in edges
 where
   point2RealToVec2Float = first (point2ToVec2 . fmap realToFrac)
   vec2FloatToPoint2Real = first (fmap realToFrac . vec2ToPoint2)

vec2ToPoint2 :: V2 a -> Point 2 a
vec2ToPoint2 (V2 x y) =
  fromMaybe (error "vec2ToPoint2: the impossible happened")
    . pointFromList $ [x, y]

point2ToVec2 :: Point 2 a -> V2 a
point2ToVec2 p =
  let v = toVec p
      (x, v') = VF.destruct v
      (y, _ ) = VF.destruct v'
  in V2 x y

vecNormal :: (Epsilon a, Floating a) => V2 a -> V2 a
vecNormal (V2 x y) = normalize $ V2 y (-x)

implies :: Bool -> Bool -> Bool
implies a b = not a || b
