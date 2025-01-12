{-# LANGUAGE DeriveFunctor #-}

module Util (CellType, blockSize, playerSize, GameState (..), Level, Cell, MoveDirection (..), JumpDirection (..), Enemy (..), EnemyAction (..), EnemyProgram) where

import Control.Monad.Free
import Graphics.Gloss

type CellType = (Char, Bool)
type Level = [Cell]
type Cell = (Point, CellType)

blockSize :: Float
blockSize = 18.0

playerSize :: Float
playerSize = 24.0

data GameState = GameState
  { currentLevel :: Level,
    position :: Point,
    playerImg :: Int,
    animationTime :: Float,
    moveDirection :: MoveDirection,
    jumpDirection :: JumpDirection,
    speedX :: Float,
    speedY :: Float,
    coinsGot :: Int,
    allCoinsNumber :: Int,
    restart :: Bool,
    enemies :: [(Enemy, EnemyProgram ())]
  }

data MoveDirection = Left | Right | None deriving (Eq)

data JumpDirection = Jump | Fall | Stay deriving (Eq)

data Enemy = Enemy
  { cell :: Cell,
    direction :: Float
  }
  deriving (Show)

data EnemyAction next
  = MoveLeft next
  | MoveRight next
  | Wait next
  deriving (Functor)

type EnemyProgram = Free EnemyAction
