module Main (main) where

import Enemy (Enemy (..), enemyBehaviorLeft, enemyBehaviorRight)
import Graphics.Gloss
import Render (render)
import Update (allCoinsLvl, handleEvent, startPlayerImg, startPosition, updateState)
import Util (GameState (..), JumpDirection (..), Level, MoveDirection (..), blockSize)

windowWidth :: Int
windowWidth = 576

windowHeight :: Int
windowHeight = 432

prepareRaw :: String -> Int -> Level
prepareRaw row y =
  [ ( ( (fromIntegral x * blockSize) - ((fromIntegral windowWidth / 2) - (blockSize / 2)),
        (fromIntegral y * blockSize) - ((fromIntegral windowHeight / 2) - (blockSize / 2))
      ),
      ( row !! x,
        True
      )
    )
    | x <- [0 .. length row - 1],
      row !! x /= '.'
  ]

prepareLevel :: [String] -> Level
prepareLevel rawData = concat [prepareRaw (rawData !! y) y | y <- [0 .. length rawData - 1]]

main :: IO ()
main = do
  earthLeftImg <- loadBMP "assets/earth_left.bmp" -- 0
  earthCenterImg <- loadBMP "assets/earth_center.bmp" -- 1
  earthRightImg <- loadBMP "assets/earth_right.bmp" -- 2
  playerLeftImg <- loadBMP "assets/player_left.bmp" -- 3
  playerLeftJumpImg <- loadBMP "assets/player_left_jump.bmp" -- 4
  playerRightImg <- loadBMP "assets/player_right.bmp" -- 5
  playerRightJumpImg <- loadBMP "assets/player_right_jump.bmp" -- 6
  flyEarthLeftImg <- loadBMP "assets/fly_earth_left.bmp" -- 7
  flyEarthCenterImg <- loadBMP "assets/fly_earth_center.bmp" -- 8
  flyEarthRightImg <- loadBMP "assets/fly_earth_right.bmp" -- 9
  flyEarthSingleImg <- loadBMP "assets/fly_earth_single.bmp" -- 10
  coinImg <- loadBMP "assets/coin.bmp" -- 11
  doorImg <- loadBMP "assets/door.bmp" -- 12
  enemyLeftImg <- loadBMP "assets/enemy_left.bmp" -- 13
  enemyRightImg <- loadBMP "assets/enemy_right.bmp" -- 14
  cloudLeftImg <- loadBMP "assets/cloud_left.bmp" -- 15
  cloudCenterImg <- loadBMP "assets/cloud_center.bmp" -- 16
  cloudRightImg <- loadBMP "assets/cloud_right.bmp" -- 17
  tree1Img <- loadBMP "assets/tree1.bmp" -- 18
  tree2Img <- loadBMP "assets/tree2.bmp" -- 19
  tree3Img <- loadBMP "assets/tree3.bmp" -- 20
  tree4Img <- loadBMP "assets/tree4.bmp" -- 21
  tree5Img <- loadBMP "assets/tree5.bmp" -- 22
  tree6Img <- loadBMP "assets/tree6.bmp" -- 23
  tree7Img <- loadBMP "assets/tree7.bmp" -- 24
  tree8Img <- loadBMP "assets/tree8.bmp" -- 25
  tree9Img <- loadBMP "assets/tree9.bmp" -- 26
  tree10Img <- loadBMP "assets/tree10.bmp" -- 27
  tree11Img <- loadBMP "assets/tree11.bmp" -- 28
  rawData <- readFile "assets/level.txt"
  let level = prepareLevel $ reverse $ lines rawData
  let state =
        GameState
          { currentLevel = level,
            position = startPosition,
            playerImg = startPlayerImg,
            animationTime = 0.00,
            moveDirection = Util.None,
            jumpDirection = Util.Stay,
            speedX = 0,
            speedY = 0,
            coinsGot = 0,
            allCoinsNumber = allCoinsLvl,
            restart = False,
            enemies =
              [ (Enemy {cell = ((40.0, -186.0), ('o', True)), direction = 1}, enemyBehaviorRight),
                (Enemy {cell = ((4.0, -132.0), ('p', True)), direction = 1}, enemyBehaviorLeft),
                (Enemy {cell = ((-150.0, -96.0), ('o', True)), direction = 1}, enemyBehaviorRight)
              ]
          }
  play
    window
    background
    60
    state
    ( `render`
        [ earthLeftImg,
          earthCenterImg,
          earthRightImg,
          playerLeftImg,
          playerLeftJumpImg,
          playerRightImg,
          playerRightJumpImg,
          flyEarthLeftImg,
          flyEarthCenterImg,
          flyEarthRightImg,
          flyEarthSingleImg,
          coinImg,
          doorImg,
          enemyLeftImg,
          enemyRightImg,
          cloudLeftImg,
          cloudCenterImg,
          cloudRightImg,
          tree1Img,
          tree2Img,
          tree3Img,
          tree4Img,
          tree5Img,
          tree6Img,
          tree7Img,
          tree8Img,
          tree9Img,
          tree10Img,
          tree11Img
        ]
    )
    handleEvent
    updateState
  where
    window = InWindow "game-like-mario" (windowWidth, windowHeight) (450, 150)
    background = makeColor 0.529 0.808 0.922 1
