module Main where

import           Draw                           ( drawTree )
import           UST                            ( randomMaze )
import           System.Environment             ( getArgs )
import qualified Data.Set                      as S
                                                ( toList )
import           Data.Random.Sample             ( sample )
import           Graphics.Gloss                 ( Picture
                                                , display
                                                , Display(FullScreen)
                                                , white
                                                , black
                                                , color
                                                , translate
                                                , scale
                                                )

scaleFactor :: Float
scaleFactor = 10

padding :: Float
padding = 0.3

main :: IO ()
main = do
  args <- getArgs
  let (w, h) = case args of
        a : b : _ -> (read a, read b)
        _         -> (100, 100)
  picture w h >>= display FullScreen white

picture :: Int -> Int -> IO Picture
picture w h = fmap draw $ sample $ randomMaze w h
 where
  center  = translate (-fromIntegral w / 2) (-fromIntegral h / 2)
  enlarge = scale scaleFactor scaleFactor
  draw    = color black . enlarge . center . drawTree padding . S.toList
