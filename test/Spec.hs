import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import UST
import qualified Data.Random.Sample as R
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

toGraph :: Ord a => Map a a -> Graph a
toGraph = foldr f M.empty . M.toList
  where add v w graph = case M.lookup v graph of
          Nothing -> M.insert v [w] graph
          Just nbhrs -> M.insert v (w:nbhrs) graph
        f (v, w) = add v w . add w v

acyclic :: Graph Coord -> Bool
acyclic graph
  | M.null graph = True
  | S.null leaves = False
  | otherwise = acyclic reducedGraph2
  where
    leaves = M.keysSet . M.filter (\nbhrs -> length nbhrs <= 1) $ graph
    reducedGraph2 = fmap (filter (`S.notMember` leaves)) . M.filterWithKey (\v _ -> v `S.notMember` leaves) $ graph
    
component :: Ord a => Graph a -> a -> Set a
component graph v = go S.empty [v]
  where go acc (v : vs) = case M.lookup v graph of
          (Just nbhrs) -> go (S.insert v acc) ((filter (`S.notMember` acc) nbhrs) ++ vs)
          _ -> go (S.insert v acc) vs
        go acc _ = acc

gridSize = 50

prop_acyclic = monadicIO $ do
  tree <- run $ R.sample $ randomUST (rectLattice gridSize gridSize) (0, 0)
  assert (acyclic $ toGraph tree)

prop_connected = monadicIO $ do
  tree <- run $ R.sample $ randomUST (rectLattice gridSize gridSize) (0, 0)
  let treeGraph = toGraph tree
      c = component treeGraph (0, 0)
  assert (S.size c == M.size treeGraph)

main = hspec $ do
  describe "wilson's algorithm" $ do
    it "returns a connected graph" $ do
      quickCheck prop_connected
    it "returns an acyclic graph" $ do
      quickCheck prop_acyclic
    
