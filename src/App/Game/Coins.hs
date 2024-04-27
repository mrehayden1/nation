module App.Game.Coins (
 Coin(..),
 coins
) where

import Control.Applicative
import Data.Tuple.Extra
import Linear
import Reflex

import App.Entity
import App.Entity.Collision
import App.Env
import App.Matrix
import App.Output

coinPlayerCollectCooldown :: Float
coinPlayerCollectCooldown = 2

-- TODO Should we move playerCoins to the player code?
coins :: forall t. Event t ()
  -> Dynamic t (V3 Float)
  -> Dynamic t (V3 Float)
  -> App t (Dynamic t [Coin], Dynamic t Int)
coins rClickE playerPosition playerDirection = do
  Entities{..} <- asks envEntities
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
    (looseCoins :: Dynamic t [Coin]) <- foldDyn ($) initialCoins
      . mergeWith (.) $ [
          fmap removeIxs coinsCollectedE,
          fmap (uncurry addCoin) coinsDroppedE
        ]
    playerCoins <- foldDyn ($) 0 . mergeWith (.) $ [
        (+) . length <$> coinsCollectedE,
        subtract 1   <$  coinsDroppedE
      ]
  return (
      looseCoins,
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

  makePlayerCollision playerE pos dir =
    let transform = translate2D (V2 px pz) !*! rotate2D rot
        (V3 px _ pz) = pos
        (V3 dx _ dz) = dir
        rot = atan (dz/dx)
    in transformCollision transform . playerECoinPickupCollision $ playerE

  findCollectedCoinsI :: CoinE -> [Coin] -> Float -> Collision -> [Int]
  findCollectedCoinsI coinEntity cs time collision =
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
      in collided collision . flip transformCollision coinCollision'
          . translate2D $ V2 x z
