module App.Geometry.Clip (
  diff,
  intersection,
  union,
  xor,

  toTristrip
) where

import Data.List (partition)
import Foreign hiding (xor)
import Foreign.C
import GHC.IO.Unsafe
import Linear

import App.Geometry.Types

#include "gpc.h"

newtype GpcOp = GpcOp { unGpcOp :: CInt }
  deriving (Eq, Show)

pattern GPC_DIFF :: GpcOp
pattern GPC_DIFF = GpcOp (#const GPC_DIFF)

pattern GPC_INT :: GpcOp
pattern GPC_INT = GpcOp (#const GPC_INT)

pattern GPC_XOR :: GpcOp
pattern GPC_XOR = GpcOp (#const GPC_XOR)

pattern GPC_UNION :: GpcOp
pattern GPC_UNION = GpcOp (#const GPC_UNION)


data GpcVertex = GpcVertex { unGpcVertex :: V2 CDouble }
  deriving (Show)

instance Storable GpcVertex where
  sizeOf _ = (#size gpc_vertex)

  alignment _ = (#alignment gpc_vertex)

  peek p = do
    (x :: CDouble) <- peekByteOff p $ (#offset gpc_vertex, x)
    (y :: CDouble) <- peekByteOff p $ (#offset gpc_vertex, y)
    return . GpcVertex $ V2 x y

  poke p (GpcVertex (V2 x y)) = do
    pokeByteOff p (#offset gpc_vertex, x) x
    pokeByteOff p (#offset gpc_vertex, y) y


data GpcVertexList = GpcVertexList { unGpcVertexList ::  [V2 CDouble] }
  deriving (Show)

instance Storable GpcVertexList where
  sizeOf _ = (#size gpc_vertex_list)

  alignment _ = (#alignment gpc_vertex_list)

  peek p = do
    (numVertices' :: CInt)
      <- peekByteOff p (#offset gpc_vertex_list, num_vertices)
    let numVertices = fromIntegral numVertices'
    verticesPtr <- peekByteOff p (#offset gpc_vertex_list, vertex)
    vertices <- fmap (fmap unGpcVertex) . peekArray numVertices $ verticesPtr
    return . GpcVertexList $ vertices

  poke p (GpcVertexList vs) = do
    let numVertices = fromIntegral . length $ vs :: CInt
    pokeByteOff p (#offset gpc_vertex_list, num_vertices) numVertices
    vertices <- newArray . fmap GpcVertex $ vs
    pokeByteOff p (#offset gpc_vertex_list, vertex) vertices


data GpcPolygon = GpcPolygon { unGpcPolygon :: [([V2 CDouble], Bool)] }
  deriving (Show)

instance Storable GpcPolygon where
  sizeOf _ = (#size gpc_polygon)

  alignment _ = (#alignment gpc_polygon)

  peek p = do
    numContours <- fmap (fromIntegral :: CInt -> Int) . peekByteOff p
      $ (#offset gpc_polygon, num_contours)
    (holesPtr :: Ptr CInt) <- peekByteOff p (#offset gpc_polygon, hole)
    holes <- fmap (fmap toBool) . peekArray numContours $ holesPtr
    contoursPtr <- peekByteOff p (#offset gpc_polygon, contour)
    contours <- fmap (fmap unGpcVertexList) . peekArray numContours
                  $ contoursPtr
    return . GpcPolygon . zip contours $ holes

  poke p (GpcPolygon faces) = do
    let numContours = fromIntegral . length $ faces :: CInt
    pokeByteOff p (#offset gpc_polygon, num_contours) numContours
    (holesPtr :: Ptr CInt) <- newArray . fmap fromBool $ holes
    pokeByteOff p (#offset gpc_polygon, hole) holesPtr
    contoursPtr <- newArray . fmap GpcVertexList $ contours
    pokeByteOff p (#offset gpc_polygon, contour) contoursPtr
   where
    (contours, holes) = unzip faces

foreign import capi "gpc.h gpc_polygon_clip" gpc_polygon_clip
  :: GpcOp -> Ptr GpcPolygon -> Ptr GpcPolygon -> Ptr GpcPolygon -> IO ()

foreign import capi "gpc.h gpc_free_polygon" gpc_free_polygon
  :: Ptr GpcPolygon -> IO ()

toGpcPolygon :: Real a => Polygon a -> GpcPolygon
toGpcPolygon p =
  let Polygon faces' holes' = fmap realToFrac p
  in GpcPolygon $ fmap (, False) faces' ++ fmap (, True) holes'

fromGpcPolygon :: Fractional a => GpcPolygon -> Polygon a
fromGpcPolygon (GpcPolygon contours) =
  let (faces, holes) = partition ((== False) . snd) contours
  in fmap realToFrac . Polygon (fmap fst faces) . fmap fst $ holes

clip :: RealFrac a => GpcOp -> Polygon a -> Polygon a -> Polygon a
clip op p q = unsafePerformIO $
  alloca $ \p' ->
    alloca $ \q' ->
      alloca $ \r' -> do
        poke p' . toGpcPolygon $ p
        poke q' . toGpcPolygon $ q
        gpc_polygon_clip op p' q' r'
        r <- peek r'
        gpc_free_polygon r'
        return . fromGpcPolygon $ r

diff :: RealFrac a => Polygon a -> Polygon a -> Polygon a
diff = clip GPC_DIFF

intersection :: RealFrac a => Polygon a -> Polygon a -> Polygon a
intersection = clip GPC_INT

union :: RealFrac a => Polygon a -> Polygon a -> Polygon a
union = clip GPC_UNION

xor :: RealFrac a => Polygon a -> Polygon a -> Polygon a
xor = clip GPC_XOR


data GpcTristrip = GpcTristrip { unGpcTristrip :: [[V2 CDouble]] }

instance Storable GpcTristrip where
  sizeOf _ = (#size gpc_tristrip)

  alignment _ = (#alignment gpc_tristrip)

  peek p = do
    numStrips <- fmap (fromIntegral :: CInt -> Int) . peekByteOff p
      $ (#offset gpc_tristrip, num_strips)
    stripPtr <- peekByteOff p (#offset gpc_tristrip, strip)
    strips <- fmap (fmap unGpcVertexList) . peekArray numStrips
                $ stripPtr
    return . GpcTristrip $ strips

  poke p (GpcTristrip strips) = do
    let numStrips = fromIntegral . length $ strips :: CInt
    pokeByteOff p (#offset gpc_tristrip, num_strips) numStrips
    stripsPtr <- newArray . fmap GpcVertexList $ strips
    pokeByteOff p (#offset gpc_tristrip, strip) stripsPtr

foreign import capi "gpc.h gpc_polygon_to_tristrip" gpc_polygon_to_tristrip
  :: Ptr GpcPolygon -> Ptr GpcTristrip -> IO ()

foreign import capi "gpc.h gpc_free_tristrip" gpc_free_tristrip
  :: Ptr GpcTristrip -> IO ()

fromGpcTristrip :: Fractional a => GpcTristrip -> Tristrip a
fromGpcTristrip = Tristrip . fmap (fmap (fmap realToFrac)) . unGpcTristrip

toTristrip:: RealFrac a => Polygon a -> Tristrip a
toTristrip p =
  unsafePerformIO $
    alloca $ \p' ->
      alloca $ \t' -> do
        poke p' . toGpcPolygon $ p
        gpc_polygon_to_tristrip p' $ t'
        t <- peek t'
        gpc_free_tristrip t'
        return . fromGpcTristrip $ t
