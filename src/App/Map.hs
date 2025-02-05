module App.Map (
  module Control.Monad.Random,

  MapData(..),
  MapGraph(..),
  MapTree(..),

  generateTestMapGeometry,
  generateMapGeometry,
  generateMapGraph,
  poiDiscRadius,

  perlinPath,
  perlinLoop
) where

import Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import Algorithms.Geometry.DelaunayTriangulation.Types
import Control.Applicative
import Control.Lens
import Control.Monad.Random
import Data.Bifunctor
import Data.Ext
import Data.Function
import Data.List (foldl', minimumBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.RealNumber.Rational
import Geometry.Point
import qualified Geometry.Vector.VectorFamily as VF
import Linear
import Numeric.Noise.Perlin

import App.Geometry
import App.Collision.AABB

data MapGrid = Center | North | NorthEast | East | SouthEast | South
                 | SouthWest | West | NorthWest
 deriving (Eq)

data MapGraph = MapGraph {
  mapGraphNodes :: [V2 Float],
  mapGraphEdges :: [(V2 Float, V2 Float)]
}

-- Map graph, rooms, paths, mesh, trees
data MapData = MapData {
  mapGraph :: MapGraph,
  mapRoomGeometry :: [Polygon Float],
  mapPathGeometry :: [Polygon Float],
  mapMesh :: Tristrip Float,
  mapTrees :: [MapTree]
}

data MapTree = MapTree {
  mapTreePosition :: V2 Float,
  mapTreeRotation :: Float
}

instance HasAABB MapTree (V3 Float) where
  aabb tree =
    let V2 x z = mapTreePosition tree
        p = V3 x 0 z
    in AABB (p + V3 (-15) 0 (-15)) (p + V3 15 0 15)

-- The radius around of point of interest in which another can't exist
poiDiscRadius :: Fractional a => a
poiDiscRadius = 15

-- Two room map for testing gameplay
generateTestMapGeometry :: Int -> MapData
generateTestMapGeometry seed = flip evalRand (mkStdGen seed) $ do
  t <- getRandomR (0, 2 * pi)
  let start = 0
      end = start + V2 (spread * sin t) (spread * cos t)
  path <- perlinPath start end
  rooms <- mapM perlinLoop [start, end]
  let graph = MapGraph [start, end] [(start, end)]
      paths = [path]
      shape = foldl' union (App.Geometry.Polygon [] []) $ rooms ++ paths
      mesh = toTristrip shape
  trees <- generateTrees shape
  return . MapData graph rooms paths mesh $ trees
 where
  spread = 50

-- Full map
generateMapGeometry :: Int -> MapData
generateMapGeometry seed = flip evalRand (mkStdGen seed) $ do
  graph@MapGraph{..} <- generateMapGraph
  rooms <- mapM perlinLoop mapGraphNodes
  paths <- mapM (uncurry perlinPath) mapGraphEdges
  let shape = foldl' union (App.Geometry.Polygon [] []) $ rooms ++ paths
      mesh = toTristrip shape
  trees <- generateTrees shape
  return . MapData graph rooms paths mesh $ trees

generateTrees :: forall g. RandomGen g
  => Polygon Float
  -> Rand g [MapTree]
generateTrees geometry = do
  let vs = [ V2 x y | x <- ps, y <- ps ]
  ps' <- fmap (filter test) . mapM addJitter $ vs
  rs <- getRandomRs (0, 2 * pi)
  return . zipWith MapTree ps' $ rs
 where
  -- Tree density
  n :: forall n. Num n => n
  n = 10 * 20

  ps = fmap ((* (2 * treeCoverage)) . subtract 0.5 . (/ n) . fromIntegral)
            [0..n :: Int]

  -- The radius within the level geometry trees should be generated
  treeDisc = 15 * 20
  -- The range in each axis over which we should generate tree geometry +/- in
  -- each axis
  treeCoverage = 30 * 20

  test = liftA2 (&&)
                (not . pointInPolygon geometry)
                -- TODO Doesn't check circle overlapping geometry edge, but
                -- geometry is tight enough to overcome this
                (flip any points . flip pointInDisc treeDisc)
   where
     points = concat . liftA2 (++) polygonFaces polygonHoles $ geometry

  addJitter :: V2 Float -> Rand g (V2 Float)
  addJitter a = do
    t <- getRandomR (0, 2*pi)
    r <- getRandomR (0, 5)
    let x = r * cos t
        y = r * sin t
    return $ a + V2 x y

perlinPath :: RandomGen g => V2 Float -> V2 Float -> Rand g (Polygon Float)
perlinPath p0 p1 = do
  route <- perlinRoute p0 p1
  let pairs = zip route . drop 1 $ route
      norms = fmap (uncurry $ (vecNormal .) . (-)) pairs
      path' = (++) (zipWith (+) route norms)
                . reverse . zipWith (-) route $ norms
  return . Polygon [path'] $ []

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
         . (/ 2) . (+ 1) . circularPerlin dist $ t

  n :: Int
  n = 100

  wiggle = 2.5

-- Make a comlpete loop by sampling a perlin distribution on the circumfrence
-- of a circle
perlinLoop :: RandomGen g => V2 Float -> Rand g (Polygon Float)
perlinLoop c = do
  seed <- getRandom
  let dist = perlin seed 2 0.5 0.5 -- seed, octaves, scale, persistence
      room = fmap (point' dist) [0..(n - 1)]
  return . Polygon [room] $ []
 where
  point' :: Perlin -> Int -> V2 Float
  point' p n' =
    let t = (/ realToFrac n) . ((2 * pi) *) . realToFrac $ n'
    in (+ c) . (*^ circlePoint t) . (+ roomSize) . (* roomDeviation)
         . circularPerlin p $ t

  roomSize = 20
  roomDeviation = 2

  -- Number of points in the loop
  n = 100

  -- The point on the unit circle at theta
  circlePoint :: (Floating a) => a -> V2 a
  circlePoint = liftA2 V2 cos sin

-- Sample a perlin distribution on the circumfrence of the unit cirlce at
-- theta to produce a value between -1 and 1
circularPerlin :: (Floating a, Real a) => Perlin -> a -> a
circularPerlin p = realToFrac . noiseValue p . liftA2 (0,,) (realToFrac . cos) (realToFrac . sin)

generateMapGraph :: RandomGen g => Rand g MapGraph
generateMapGraph = do
  (ps :: [V2 Float :+ MapGrid]) <- generatePointsOfInterest
  let edges = validEdges . triangulate $ ps
  return . MapGraph (fmap _core ps) $ fmap (bimap _core _core) edges
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
        rs <- fmap (take 10000) getRandoms
        let p' = head . filter valid . fmap (subtract 0.5) $ rs
        addPoints' (p' : ps') n xMin xMax yMin yMax ps
   where
    valid :: V2 a -> Bool
    valid = liftA2 (&&)
              (pointInAabb (V2 xMin yMin) (V2 xMax yMax))
              (not . flip any (ps <> ps') . flip pointInDisc poiDiscRadius)

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
