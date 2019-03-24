module UST where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Random                    ( RVar )
import           Data.Random.List               ( randomElement )

type Graph a = Map a [a]
type Coord = (Int, Int)

rectLattice :: Int -> Int -> Graph (Int, Int)
rectLattice w h = M.fromSet nbhrs vertices
 where
  vertices = S.fromList [ (i, j) | i <- [0 .. w], j <- [0 .. h] ]
  nbhrs (i, j) = filter inRange [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
  inRange (i, j) = 0 <= i && i <= w && 0 <= j && j <= h

randomNbhr :: Ord a => Graph a -> a -> RVar (Maybe a)
randomNbhr graph v = case M.lookup v graph of
  Just nbhrs | not (null nbhrs) -> Just <$> randomElement nbhrs
  _                             -> return Nothing

randomLERW :: Ord a => Graph a -> Set a -> a -> RVar (Map a a)
randomLERW graph seen v0 = go v0 M.empty
  where go v walk = randomNbhr graph v >>= \nbhr -> case nbhr of
          Just w | v `S.notMember` seen -> go w (M.insert v w walk)
          _ -> return walk

randomUST :: Ord a => Graph a -> a -> RVar (Map a a)
randomUST graph seed = randomUSTHelper graph seen unseen 
  where
    vertices = M.keysSet graph
    seen = S.singleton seed
    unseen = S.delete seed vertices

randomUSTHelper :: Ord a => Graph a -> Set a -> Set a -> RVar (Map a a)
randomUSTHelper graph seen unseen = case S.lookupMax unseen of
  Just v -> do
    branch <- randomLERW graph seen v
    let newlySeen = exploreFrom v branch
    restOfTree <- randomUSTHelper graph (S.union seen newlySeen) (S.difference unseen newlySeen)
    return $ M.union restOfTree branch

  _ -> return M.empty
  where
    exploreFrom v walk = case M.lookup v walk of
      (Just w) -> S.insert v (exploreFrom w walk)
      _ -> S.singleton v

randomMaze :: Int -> Int -> RVar [(Coord, Coord)]
randomMaze w h = M.toList <$> randomUST (rectLattice w h) (0, 0)
