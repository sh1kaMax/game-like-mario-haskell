module Collision (isHitX, isHitUp, isHitDown, isWallPlayerNeeded, isCoinNeeded, isCollision, isDoorNeeded, isWallNeeded) where

import Graphics.Gloss
import Util (CellType, GameState (..), blockSize, playerSize)

isHitX :: Point -> Point -> Bool -> Bool
isHitX (x1, y1) (x2, y2) flag =
  flag
    && ( (x1 - 10 < x2 + blockSize / 2 && x1 - 10 > x2 - blockSize / 2)
           || (x1 + 10 < x2 + blockSize / 2 && x1 + 10 > x2 - blockSize / 2)
       )
    && ( (y2 + blockSize / 2 < y1 + playerSize / 2 && y2 + blockSize / 2 > y1 - playerSize / 2)
           || (y2 - blockSize / 2 < y1 + playerSize / 2 && y2 - blockSize / 2 > y1 - playerSize / 2)
       )

isHitUp :: Point -> Point -> Bool -> Bool
isHitUp (x1, y1) (x2, y2) flag =
  flag
    && ( (x2 - blockSize / 2 > x1 - 10 && x2 - blockSize / 2 < x1 + 10)
           || (x2 + blockSize / 2 > x1 - 10 && x2 + blockSize / 2 < x1 + 10)
       )
    && (y1 + playerSize / 2 > y2 - blockSize / 2 && y1 + playerSize / 2 < y2 + blockSize / 2)

isHitDown :: Point -> Point -> Bool -> Bool
isHitDown (x1, y1) (x2, y2) flag =
  flag
    && ( (x2 - blockSize / 2 > x1 - 10 && x2 - blockSize / 2 < x1 + 10)
           || (x2 + blockSize / 2 > x1 - 10 && x2 + blockSize / 2 < x1 + 10)
       )
    && (y1 - playerSize / 2 > y2 - blockSize / 2 && y1 - playerSize / 2 < y2 + blockSize / 2)

isWallPlayerNeeded :: CellType -> Bool
isWallPlayerNeeded cell = fst cell /= '*' && fst cell /= '!' && fst cell /= 'w' && fst cell /= 'd' && fst cell /= 'g' && fst cell /= 'l' && fst cell /= 'c' && fst cell /= 'r'

isWallNeeded :: CellType -> Bool
isWallNeeded cell = fst cell /= '*' && fst cell /= '!' && fst cell /= 'd' && fst cell /= 'g'

isCoinNeeded :: CellType -> Bool
isCoinNeeded cell = fst cell == '*'

isDoorNeeded :: CellType -> Bool
isDoorNeeded cell = fst cell == '!'

isCollision :: (Point -> Point -> Bool -> Bool) -> (CellType -> Bool) -> Point -> GameState -> Bool
isCollision func func2 point gs =
  any
    (\((x, y), cellType) -> func point (x, y) (func2 cellType))
    (currentLevel gs)
