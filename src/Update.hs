module Update (handleEvent, updateState, startPosition, startPlayerImg, allCoinsLvl) where

import Collision (isCoinNeeded, isCollision, isDoorNeeded, isHitX)
import Enemy (enemyBehaviorLeft, enemyBehaviorRight, interpretEnemy)
import Graphics.Gloss.Interface.IO.Game
import Player (isDie, moveX, moveY, updateJumpDirection, updatePlayerImg, updateSpeedX, updateSpeedY)
import Util (Enemy (..), GameState (..), JumpDirection (..), Level, MoveDirection (..))

startPosition :: Point
startPosition = (4.0, -186.0)

startPlayerImg :: Int
startPlayerImg = 5

allCoinsLvl :: Int
allCoinsLvl = 7

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'a') Down _ _) gs =
  gs {moveDirection = Util.Left, speedX = updateSpeedX (speedX gs) (moveDirection gs == Util.Left)}
handleEvent (EventKey (Char 'd') Down _ _) gs =
  gs {moveDirection = Util.Right, speedX = updateSpeedX (speedX gs) (moveDirection gs == Util.Right)}
handleEvent (EventKey (Char 'w') Down _ _) gs = if jumpDirection gs == Util.Stay then gs {speedY = 3.5, jumpDirection = Util.Jump} else gs
handleEvent (EventKey (Char 'f') Down _ _) gs = if isCollision isHitX isDoorNeeded (position gs) gs && coinsGot gs == allCoinsNumber gs then gs {restart = True} else gs
handleEvent (EventKey (Char key) Up _ _) gs =
  if (moveDirection gs == Util.Right && key == 'd')
    || (moveDirection gs == Util.Left && key == 'a')
    then gs {moveDirection = Util.None, speedX = 0}
    else gs
handleEvent _ gs = gs {moveDirection = Util.None, speedX = 0}

updateAnimationTime :: GameState -> Float -> Float
updateAnimationTime gs dt
  | animationTime gs < 0.1 = animationTime gs + dt
  | otherwise = 0.00

updateLevel :: Level -> Point -> Level
updateLevel lvl playerPos =
  [ if isHitX playerPos cellPos (isCoinNeeded cellType)
      then (cellPos, (fst cellType, False))
      else cellTo
    | cellTo@(cellPos, cellType) <- lvl
  ]

updateCoinsGot :: Level -> Point -> Int
updateCoinsGot lvl playerPos =
  if any (\(cellPos, cellType) -> isHitX playerPos cellPos (isCoinNeeded cellType) && snd cellType) lvl
    then 1
    else 0

resetLvl :: Level -> Level
resetLvl lvl =
  [(cellPoss, (fst cellType, True)) | (cellPoss, cellType) <- lvl]

updateState :: Float -> GameState -> GameState
updateState dt gs =
  if not $ restart gs
    then
      gs
        { position = newPosition,
          playerImg = updatePlayerImg (speedX gs) (playerImg gs) gs,
          animationTime = updateAnimationTime gs dt,
          speedY = newSpeedY,
          coinsGot = coinsGot gs + updateCoinsGot (currentLevel gs) newPosition,
          jumpDirection = updateJumpDirection (jumpDirection gs) maybeNewPointY gs,
          currentLevel = updateLevel (currentLevel gs) newPosition,
          enemies = map (uncurry (interpretEnemy gs)) (enemies gs),
          restart = isDie newPosition gs
        }
    else
      gs
        { currentLevel = resetLvl (currentLevel gs),
          position = startPosition,
          playerImg = startPlayerImg,
          animationTime = 0.00,
          moveDirection = Util.None,
          jumpDirection = Util.Stay,
          speedX = 0.00,
          speedY = 0.00,
          coinsGot = 0,
          allCoinsNumber = allCoinsLvl,
          restart = False,
          enemies =
            [ (Enemy {cell = ((40.0, -186.0), ('o', True)), direction = 1}, enemyBehaviorRight),
              (Enemy {cell = ((4.0, -132.0), ('p', True)), direction = 1}, enemyBehaviorLeft),
              (Enemy {cell = ((-150.0, -96.0), ('o', True)), direction = 1}, enemyBehaviorRight)
            ]
        }
  where
    maybeNewPointY = (fst $ position gs, snd (position gs) + speedY gs - 0.1)
    newPosition = moveY (speedY gs) gs $ moveX (moveDirection gs) gs
    newSpeedY = updateSpeedY (jumpDirection gs) maybeNewPointY gs
