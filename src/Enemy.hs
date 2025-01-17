module Enemy (Enemy (..), EnemyAction (..), EnemyProgram, enemyBehaviorLeft, enemyBehaviorRight, interpretEnemy) where

import Collision (isCollision, isHitX, isWallNeeded)
import Control.Monad.Free
import Util (Enemy (..), EnemyAction (..), EnemyProgram, GameState)

moveLeft :: EnemyProgram ()
moveLeft = liftF $ MoveLeft ()

moveRight :: EnemyProgram ()
moveRight = liftF $ MoveRight ()

wait :: EnemyProgram ()
wait = liftF $ Wait ()

enemyBehaviorLeft :: EnemyProgram ()
enemyBehaviorLeft = do
  moveLeft
  wait
  moveRight
  wait
  enemyBehaviorLeft

enemyBehaviorRight :: EnemyProgram ()
enemyBehaviorRight = do
  moveRight
  wait
  moveLeft
  wait
  enemyBehaviorRight

interpretEnemy :: GameState -> Enemy -> EnemyProgram () -> (Enemy, EnemyProgram ())
interpretEnemy gs enemy (Free (MoveLeft next)) =
  let (x, y) = fst (cell enemy)
      newPosition = (x - 0.3, y)
   in if isCollision isHitX isWallNeeded newPosition gs
        then (enemy {cell = ((x, y), ('o', snd $ snd $ cell enemy))}, next)
        else (enemy {cell = ((x - 0.5, y), snd $ cell enemy)}, Free (MoveLeft next))
interpretEnemy gs enemy (Free (MoveRight next)) =
  let (x, y) = fst (cell enemy)
      newPosition = (x + 0.3, y)
   in if isCollision isHitX isWallNeeded newPosition gs
        then (enemy {cell = ((x, y), ('p', snd $ snd $ cell enemy))}, next)
        else (enemy {cell = ((x + 0.5, y), snd (cell enemy))}, Free (MoveRight next))
interpretEnemy _ enemy (Free (Wait next)) =
  (enemy, next)
interpretEnemy _ enemy (Pure _) =
  (enemy, Pure ())
