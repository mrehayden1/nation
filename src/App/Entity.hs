module App.Entity (
  Entity(..),

  coinCollision,
  peasantCollision,
  peasantCoinVision,
  playerCollision,
  playerCoinPickupCollision
) where

import Linear

import App.Collision

data Entity = EntityCoin
  | EntityGrass
  | EntityOakTree
  | EntityPeasant
  | EntityPlayer
  | EntityPointer
 deriving (Eq, Ord, Show)

coinCollision :: Fractional a => Collision2 a
coinCollision = CollisionCircle 0 0.62

peasantCollision :: Fractional a => Collision2 a
peasantCollision = CollisionPolygon [
    V2 (-0.6) (-0.3),
    V2   0.6  (-0.3),
    V2   0.6    0.3 ,
    V2 (-0.6)   0.3
  ]

peasantCoinVision :: Fractional a => Collision2 a
peasantCoinVision = CollisionCircle 0 3

playerCollision :: Fractional a => Collision2 a
playerCollision = CollisionPolygon [
    V2 (-1)   (-0.33),
    V2 (-1)     0.33 ,
    V2   1.61   0.33 ,
    V2   1.61 (-0.33)
  ]

playerCoinPickupCollision :: Fractional a => Collision2 a
playerCoinPickupCollision = CollisionCircle 0 1
