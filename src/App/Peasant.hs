module App.Peasant (
  peasants
) where

import Data.Function
import Data.List (sortBy)
import Linear
import Reflex

import App.Env
import App.Input
import App.Output
import Entity
import Entity.Collision
import Matrix
import Vector

startDirection :: V3 Float
startDirection = V3 1 0 0

startPosition :: V3 Float
startPosition = V3 5 0 0

coinCollectCooldown :: Float
coinCollectCooldown = 2

peasantMaxSpeed :: Float
peasantMaxSpeed = 2

peasants :: Dynamic t [Coin] -> App t (Dynamic t [Peasant])
peasants coins = do
  Entities{..} <- asks envEntities
  time <- asks envTime
  deltaT <- asks (fmap inputDeltaT . envInputE)
  rec
    velocity <- holdDyn 0
                  . fmap (uncurry peasantVelocity)
                  . tag (current $ (,) <$> coinsVisible <*> position)
                  $ deltaT
    direction <- holdDyn startDirection . ffilter (/= 0) . updated $ velocity
    position <- foldDyn (+) startPosition
                  . attachPromptlyDynWith (^*) velocity
                  $ deltaT
    let coinsVisible = findVisibleCoins entitiesCoin entitiesPeasant <$> coins
                         <*> time <*> direction <*> position
  return . fmap (:[]) $ Peasant <$> direction <*> position <*> velocity

 where
  peasantVelocity :: [Coin] -> V3 Float -> V3 Float
  peasantVelocity []      _   = V3 0 0 0
  peasantVelocity (c : _) pos =
    let d = coinPosition c - pos
    in if magnitude d > epsilon
         then normalize d ^* peasantMaxSpeed
         else V3 0 0 0
   where
    epsilon = 0.01

  findVisibleCoins :: CoinE
    -> PeasantE
    -> [Coin]
    -> Float
    -> V3 Float
    -> V3 Float
    -> [Coin]
  findVisibleCoins coinEntity peasantEntity cs time direction position =
    sortBy (compare `on` (distance position . coinPosition))
      . filter shouldCollect $ cs
   where
    collision = makeCollision peasantEntity position direction

    shouldCollect = liftA2
      (&&)
      ((time >) . (+ coinCollectCooldown) . coinTimeDropped)
      coinCollided

    coinCollided :: Coin -> Bool
    coinCollided Coin{..} =
      let V3 x _ z = coinPosition
          coinCollision' = coinECollision coinEntity
      in collided collision . flip transformCollision coinCollision'
          . translate2D $ V2 x z

  makeCollision peasantE pos dir =
    let transform = translate2D (V2 px pz) !*! rotate2D rot
        (V3 px _ pz) = pos
        (V3 dx _ dz) = dir
        rot = atan (dz/dx)
    in transformCollision transform . peasantECoinVision $ peasantE
