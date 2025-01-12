module Player (updateSpeedX, updateSpeedY, moveX, moveY, updatePlayerImg, updateJumpDirection, isDie) where

import Collision (isCollision, isHitDown, isHitUp, isHitX, isWallPlayerNeeded)
import Graphics.Gloss.Interface.IO.Game
import Render (GameState (..))
import Util (Enemy (..), JumpDirection (..), MoveDirection (..))

moveX :: MoveDirection -> GameState -> Point
moveX Util.Left gs =
  if isCollision isHitX isWallPlayerNeeded newPosition gs
    then position gs
    else newPosition
  where
    newPosition = (fst (position gs) - speedX gs, snd (position gs))
moveX Util.Right gs =
  if isCollision isHitX isWallPlayerNeeded newPosition gs
    then position gs
    else newPosition
  where
    newPosition = (fst (position gs) + speedX gs, snd (position gs))
moveX Util.None gs = position gs

moveY :: Float -> GameState -> Point -> Point
moveY speed gs point
  | speed > 0 =
      if isCollision isHitUp isWallPlayerNeeded newPosition gs
        then point
        else newPosition
  | speed < 0 =
      if isCollision isHitDown isWallPlayerNeeded newPosition gs
        then point
        else newPosition
  | otherwise = point
  where
    newPosition = (fst point, snd point + speed)

isDie :: Point -> GameState -> Bool
isDie playerPos gs =
  any (\enemy -> isHitX playerPos (fst $ cell (fst enemy)) True) (enemies gs)
    || fst playerPos < (-width)
    || fst playerPos > width
    || snd playerPos < (-height)
    || snd playerPos > height
  where
    width = 576.0 / 2
    height = 432.0 / 2

updateSpeedX :: Float -> Bool -> Float
updateSpeedX speed sameDict =
  if sameDict
    then
      if speed < 10
        then speed + 1
        else 10
    else 1

updateSpeedY :: JumpDirection -> Point -> GameState -> Float
updateSpeedY jumpDirect newPoint gs
  | jumpDirect == Util.Jump = if isCollision isHitUp isWallPlayerNeeded newPoint gs then -0.1 else prevSpeed - 0.1
  | jumpDirect == Util.Fall = if isCollision isHitDown isWallPlayerNeeded newPoint gs then 0.0 else prevSpeed - 0.1
  | jumpDirect == Util.Stay = 0.0
  | otherwise = 0.0
  where
    prevSpeed = speedY gs

nextImg :: Int -> MoveDirection -> Int
nextImg prev direct
  | direct == Util.Right = if prev == 5 then 6 else 5
  | direct == Util.Left = if prev == 3 then 4 else 3
  | otherwise = prev

updatePlayerImg :: Float -> Int -> GameState -> Int
updatePlayerImg speed numberImg gs
  | speed > 0 && animationTime gs >= 0.1 = nextImg numberImg (moveDirection gs)
  | speed == 0 && numberImg == 6 = 5
  | speed == 0 && numberImg == 4 = 3
  | otherwise = numberImg

updateJumpDirection :: JumpDirection -> Point -> GameState -> JumpDirection
updateJumpDirection jumpDirect newPoint gs
  | jumpDirect == Util.Jump && (collisionUp || (not collisionUp && (prevSpeed - 0.1) < 0.0)) = Util.Fall
  | jumpDirect == Util.Fall && collisionDown = Util.Stay
  | jumpDirect == Util.Stay && not collisionDown = Util.Fall
  | otherwise = jumpDirect
  where
    collisionUp = isCollision isHitUp isWallPlayerNeeded newPoint gs
    collisionDown = isCollision isHitDown isWallPlayerNeeded newPoint gs
    prevSpeed = speedY gs
