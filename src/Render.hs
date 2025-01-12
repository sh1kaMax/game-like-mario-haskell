module Render (GameState (..), render) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Util (Cell, Enemy (..), GameState (..))

whatImg :: Cell -> [Picture] -> Maybe Picture
whatImg (_, cellType) allPictures
  | fst cellType == '<' = Just $ head allPictures
  | fst cellType == '>' = Just $ allPictures !! 2
  | fst cellType == '(' = Just $ allPictures !! 7
  | fst cellType == '=' = Just $ allPictures !! 8
  | fst cellType == ')' = Just $ allPictures !! 9
  | fst cellType == '0' = Just $ allPictures !! 10
  | fst cellType == '*' = Just $ allPictures !! 11
  | fst cellType == '!' = Just $ allPictures !! 12
  | fst cellType == 'p' = Just $ allPictures !! 13
  | fst cellType == 'o' = Just $ allPictures !! 14
  | fst cellType == 'l' = Just $ allPictures !! 15
  | fst cellType == 'c' = Just $ allPictures !! 16
  | fst cellType == 'r' = Just $ allPictures !! 17
  | fst cellType == '1' = Just $ allPictures !! 18
  | fst cellType == '2' = Just $ allPictures !! 19
  | fst cellType == '3' = Just $ allPictures !! 20
  | fst cellType == '4' = Just $ allPictures !! 21
  | fst cellType == '5' = Just $ allPictures !! 22
  | fst cellType == '6' = Just $ allPictures !! 23
  | fst cellType == '7' = Just $ allPictures !! 24
  | fst cellType == '8' = Just $ allPictures !! 25
  | fst cellType == '9' = Just $ allPictures !! 26
  | fst cellType == 'g' = Just $ allPictures !! 27
  | fst cellType == 'd' = Just $ allPictures !! 28
  | fst cellType == '-' = Just $ allPictures !! 1
  | otherwise = Nothing

drawTile :: Cell -> [Picture] -> Picture
drawTile cellToPrint allPictures =
  let defaultPicture = blank
   in uncurry translate (fst cellToPrint) (fromMaybe defaultPicture (whatImg cellToPrint allPictures))

render :: GameState -> [Picture] -> Picture
render gs allPictures =
  pictures
    ( [drawTile cellToPrint allPictures | cellToPrint <- currentLevel gs, snd $ snd cellToPrint]
        ++ [uncurry translate (position gs) (allPictures !! playerImg gs)]
        ++ [uncurry translate (fst $ cell (fst enemy)) (fromMaybe blank (whatImg (cell (fst enemy)) allPictures)) | enemy <- enemies gs]
    )
