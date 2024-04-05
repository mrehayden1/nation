module App.Coins (
 Coin(..),
 coins
) where

import Data.Tuple.Extra
import Linear
import Reflex

import App.Env
import Entity
import Entity.Collision
import Matrix

coinPlayerCollectCooldown :: Float
coinPlayerCollectCooldown = 2

data Coin = Coin {
  coinTimeDropped :: Float,
  coinPosition :: V3 Float
} deriving (Show)

coins :: forall t. Event t ()
  -> Dynamic t (V3 Float)
  -> Dynamic t (V3 Float)
  -> App t (Dynamic t [V3 Float], Dynamic t Int)
coins rClickE playerPosition playerDirection = do
  Entities{..} <- asks envEntities
  deltaT <- asks (fmap inputDeltaT . envInputE)
  time <- asks envTime
  rec
    let playerCollision = liftA2 (makePlayerCollision entitiesPlayer)
                                 playerPosition
                                 playerDirection
        coinsCollectedE =
          attachWith (uncurry $ findCollectedCoinsI entitiesCoin)
                     (current $ (,) <$> looseCoins <*> time)
            . updated $ playerCollision
        coinsDroppedE =
          fmap ((,) <$> thd3 <*> snd3)
            . ffilter ((> 0) . fst3)
            . tag (current $ (,,) <$> playerCoins <*> playerPosition <*> time)
            $ rClickE
    (looseCoins :: Dynamic t [Coin]) <- foldDyn (flip (foldr ($))) initialCoins
      . mergeList $ [
          fmap removeIxs coinsCollectedE,
          fmap (uncurry addCoin) coinsDroppedE,
          fmap (const id) deltaT
        ]
    playerCoins <- foldDyn (flip (foldr ($))) 0 . mergeList $ [
        (+) . length <$> coinsCollectedE,
        subtract 1   <$  coinsDroppedE
      ]
  return (
      fmap (fmap coinPosition) looseCoins,
      playerCoins
    )
 where
  initialCoins :: [Coin]
  initialCoins = fmap (Coin 0) [
      V3   5  0   5,
      V3 (-5) 0   5,
      V3 (-5) 0 (-5),
      V3   5  0 (-5)
    ]

  removeIxs :: [Int] -> [a] -> [a]
  removeIxs is = fmap snd . filter (not . (`elem` is) . fst) . zip [0..]

  addCoin = ((:) .) . Coin

  makePlayerCollision player pos dir =
    let playerTransform = translate2D (V2 px pz) !*! rotate2D rot
        (V3 px _ pz) = pos
        (V3 dx _ dz) = dir
        rot = atan (dz/dx)
    in transformCollision playerTransform . playerECollision $ player

  findCollectedCoinsI :: CoinE -> [Coin] -> Float -> Collision -> [Int]
  findCollectedCoinsI coinEntity cs time playerCollision =
    fmap fst . filter (shouldCollect . snd) . zip [0..] $ cs
   where
    shouldCollect = liftA2
      (&&)
      ((time >) . (+ coinPlayerCollectCooldown) . coinTimeDropped)
      coinCollided
    coinCollided :: Coin -> Bool
    coinCollided Coin{..} =
      let V3 x _ z = coinPosition
          coinCollision' = coinECollision coinEntity
      in collided playerCollision . flip transformCollision coinCollision'
          . translate2D $ V2 x z
