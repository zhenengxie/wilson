module Main where

import System.Environment
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Random
import Data.Random.List
import Data.Random.Sample
import Control.Monad.State
import Graphics.Gloss

scaleFactor :: Float
scaleFactor = 10

padding :: Float
padding = 0.3

main :: IO ()
main = do
  args <- getArgs
  let (w, h) = case args of
        a : b : _ -> (read a, read b)
        _ -> (100, 100)
  picture w h >>= display FullScreen white

picture :: Int -> Int -> IO Picture
picture w h =  fmap draw $ sample $ randomMaze w h
  where center = translate (- fromIntegral w / 2) (- fromIntegral h / 2)
        enlarge = scale scaleFactor scaleFactor 
        draw = color black . enlarge . center . drawTree padding . S.toList

type Graph a = Map a [a]
type Coord = (Int, Int)

randomMaze :: Int -> Int -> RVar (Set (Coord, Coord))
randomMaze w h = randomUST (S.singleton (0, 0)) (S.delete (0, 0) vertices) grid
  where grid = rectLattice w h
        vertices = M.keysSet grid

randomUST :: Ord a => Set a -> Set a -> Graph a -> RVar (Set (a, a))
randomUST seen unseen graph = case S.lookupMin unseen of -- minimum is just a way of choosing an arb element
  Just initial -> do
    walk <- hittingPath graph seen initial initial M.empty
    let newlySeen = foldr (\(v, w) s -> S.insert v . S.insert w $ s) S.empty walk
    partialPath <- randomUST (S.union newlySeen seen) (S.difference unseen newlySeen) graph
    return $ S.union walk partialPath
  _ -> return S.empty

randomNbhr :: Ord a => Graph a -> a -> RVar (Maybe a)
randomNbhr graph v = case M.lookup v graph of
  Just nbhrs | not (null nbhrs) -> Just <$> randomElement nbhrs
  _ -> return Nothing

hittingPath :: Ord a => Graph a -> Set a -> a -> a -> Map a a -> RVar (Set (a, a))
hittingPath graph seen initial v walk
  | v `S.member` seen = return $ realiseWalk initial walk
  | otherwise = do
      nbhr <- randomNbhr graph v
      case nbhr of
        (Just w) -> hittingPath graph seen initial w (M.insert v w walk)
        _ -> return $ realiseWalk initial walk

realiseWalk :: Ord a => a -> Map a a -> (Set (a, a))
realiseWalk v walk = case M.lookup v walk of
  Just w -> S.insert (v, w) (realiseWalk w walk)
  Nothing -> S.empty

rectLattice :: Int -> Int -> Graph (Int, Int)
rectLattice w h = M.fromSet nbhrs vertices
  where
    vertices = S.fromList [(i, j) | i <- [0..w], j <- [0..h]]
    nbhrs (i, j) = filter inRange [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
    inRange (i, j) = 0 <= i && i <= w && 0<= j && j <= h

drawEdge :: Float -> ((Int, Int), (Int, Int)) -> Picture
drawEdge pad ((i, j), (k, l)) = translate (fromIntegral i) (fromIntegral j) $ case (k - i, l - j) of
  (0, 1) -> rect
  (1, 0) -> rotate 90 rect
  (0, -1) -> rotate 180 rect
  (-1, 0) -> rotate 270 rect
  _ -> error "edge should not exist"
  where
    rect = polygon [(-pad, -pad), (pad, -pad), (pad, 1 + pad), (-pad, 1 + pad)]

drawTree :: Float -> [(Coord, Coord)] -> Picture
drawTree pad = pictures . map (drawEdge pad)

testPath = [((0, 0), (1, 0)), ((0, 1), (1, 1))]
