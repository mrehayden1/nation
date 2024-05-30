module App.Game.Coins (
 Coin(..),
 coins
) where

import Control.Applicative
import Control.Lens
import Data.Tuple.Extra
import Linear
import Reflex

import App.Collision
import App.Entity
import App.Env
import App.Matrix
import App.Output

coinPlayerCollectCooldown :: Float
coinPlayerCollectCooldown = 2

-- TODO Move playerCoins to the player code?
coins :: forall t. Event t ()
  -> Dynamic t (V3 Float)
  -> Dynamic t (V3 Float)
  -> App t (Dynamic t [Coin], Dynamic t Int)
coins rClickE playerPosition playerDirection = do
  time <- asks envTime
  rec
    let pickupCollision = liftA2 transformPlayerCollision
                                 playerPosition
                                 playerDirection
        coinsCollectedE =
          attachWith (uncurry findCollectedCoinsI)
                     (current $ (,) <$> looseCoins <*> time)
            . updated $ pickupCollision
        coinsDroppedE =
          fmap ((,) <$> thd3 <*> snd3)
            . ffilter ((> 0) . fst3)
            . tag (current $ (,,) <$> playerCoins <*> playerPosition <*> time)
            $ rClickE
    looseCoins :: Dynamic t [Coin] <- foldDyn ($) initialCoins
      . mergeWith (.) $ [
          fmap removeIxs coinsCollectedE,
          fmap (uncurry addCoin) coinsDroppedE
        ]
    playerCoins <- foldDyn ($) 0 . mergeWith (.) $ [
        (+) . length <$> coinsCollectedE,
        subtract 1   <$  coinsDroppedE
      ]
  return (looseCoins, playerCoins)
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

  transformPlayerCollision pos dir =
    let transform' = translate2D (V2 px pz) !*! rotate2D rot
        (V3 px _ pz) = pos
        (V3 dx _ dz) = dir
        rot = atan (dz/dx)
    in transformCollision2 transform' playerCoinPickupCollision

  findCollectedCoinsI :: [Coin] -> Float -> Collision2 Float -> [Int]
  findCollectedCoinsI cs time pickupCollision =
    fmap fst . filter (shouldCollect . snd) . zip [0..] $ cs
   where
    shouldCollect = liftA2 (&&)
      ((time >) . (+ coinPlayerCollectCooldown) . coinTimeDropped)
      coinCollided
    coinCollided :: Coin -> Bool
    coinCollided Coin{..} =
      collided2 pickupCollision . flip transformCollision2 coinCollision
        . translate2D . (^. _xz) $ coinPosition
