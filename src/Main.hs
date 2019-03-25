{-# LANGUAGE TemplateHaskell #-} 

module Main where

import           Draw                           ( drawTree )
import           UST                            
import           System.Environment             ( getArgs )
import Data.Set (Set)
import qualified Data.Set                      as S
import Data.Map (Map)
import qualified Data.Map as M
import           Data.Random.Sample             ( sample, sampleState )
import Data.Random.Source.StdGen
import           Graphics.Gloss                 
import           Graphics.Gloss.Data.ViewPort                 
import Lens.Micro.Platform

data Model = Model { _width :: Int,
                     _height :: Int,
                     _graph :: Graph Coord,
                     _unseen :: Set Coord,
                     _tree :: Map Coord Coord,
                     _walk :: Map Coord Coord,
                     _startVertex :: Coord,
                     _currentVertex :: Coord,
                     _randomSeed :: StdGen }

makeLenses ''Model

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
        _         -> (30, 30)

  model <- initialModel w h

  simulate FullScreen white 5000 model draw step

picture :: Int -> Int -> IO Picture
picture w h = (correct w h . drawTree padding) <$> (sample $ randomMaze w h)

correct :: Int -> Int -> Picture -> Picture
correct w h = enlarge . center
 where
  center  = translate (-fromIntegral w / 2) (-fromIntegral h / 2)
  enlarge = scale scaleFactor scaleFactor

draw :: Model -> Picture
draw model = correct (model ^. width) (model ^. height) . pictures $ [color black treePicture, color red walkPicture]
  where
    treePicture = drawTree padding (M.toList $ model ^. tree)
    walkPicture = drawTree padding (M.toList $ loopErase (model ^. walk) (model ^. startVertex))

loopErase :: Ord a => Map a a -> a -> Map a a
loopErase walk v = go v M.empty
  where go v acc = case M.lookup v walk of
          Just w | w `M.notMember` acc -> go w (M.insert v w acc)
                 | otherwise -> M.insert v w acc
          _ -> acc

explore :: Ord a => Map a a -> a -> Set a
explore t v = case M.lookup v t of
  (Just w) -> S.insert v (explore t w)
  _ -> S.singleton v

initialModel :: Int -> Int -> IO Model
initialModel w h = do
  seed <- newStdGen
  let lattice = rectLattice w h
  return $ Model w h lattice (S.delete (0, 0) $ M.keysSet lattice) M.empty M.empty (0, 0) (0, 0) seed

step :: ViewPort -> Float -> Model -> Model
step _ _ model =
  let
    v = model ^. currentVertex
    (nbhr, newSeed) = sampleState (randomNbhr (model ^. graph) v) (model ^. randomSeed)
  in case nbhr of
    Just w | v `S.member` (model ^. unseen) -> (over walk $ M.insert v w) .
                                               (set currentVertex w) .
                                               (set randomSeed newSeed) $ model
    _ ->
      let
        newVertices = explore (model ^. walk) (model ^. startVertex)
        branch = loopErase (model ^. walk) (model ^. startVertex)
        newUnseen = S.difference (model ^. unseen) newVertices
        newModel = (set unseen newUnseen) .
                   (over tree $ M.union branch) .
                   (set walk M.empty) .
                   (set randomSeed newSeed) $ model
      in
        case S.lookupMin newUnseen of
          Nothing -> newModel
          Just x -> (set startVertex x) . (set currentVertex x) $ newModel
