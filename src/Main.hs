module Main where

import           Draw                           ( drawTree )
import           UST                            
import           System.Environment             ( getArgs )
import qualified Data.Set                      as S
import qualified Data.Map as M
import           Data.Random.Sample             ( sample )
import           Graphics.Gloss                 

scaleFactor :: Float
scaleFactor = 10

padding :: Float
padding = 0.35

lineColor = black
backgroundColor = white

main :: IO ()
main = do
  args <- getArgs
  let (w, h) = case args of
        a : b : _ -> (read a, read b)
        _         -> (100, 100)
  picture w h >>= display FullScreen backgroundColor

picture :: Int -> Int -> IO Picture
picture w h = do
  maze <- sample $ randomMaze w h
  return $ draw maze
 where
  center  = translate (-fromIntegral w / 2) (-fromIntegral h / 2)
  enlarge = scale scaleFactor scaleFactor
  draw    = color lineColor . enlarge . center . drawTree padding
