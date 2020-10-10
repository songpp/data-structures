{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Maze where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Array (Array, bounds, (!), (//))
import Data.Array.IO (IOArray)
import qualified Data.Array.MArray as M (freeze, newArray, writeArray)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Maybe (catMaybes)
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq
import System.Random.MWC as R

data Cell = Empty | Barrier | Start | Goal | Path deriving (Enum, Ord, Eq)

instance Show Cell where
  showsPrec _d c = showString $ case c of
    Empty -> " "
    Barrier -> "❖"
    Start -> "S"
    Goal -> "G"
    Path -> "◻︎"

type Location = (Int, Int)

type Index = Location

newtype Matrix = Matrix (Array Index Cell) deriving (Eq)

instance Show Matrix where
  showsPrec _d (Matrix arr) = showString . unlines $ extraInfo : "--------" : map textRepresentation (toSimpleArray arr)
    where
      extraInfo = show $ bounds arr

data Maze = Maze Location Location Matrix deriving (Eq, Show)

defaultSparseness :: Int
defaultSparseness = 25

-- 生成随机的迷宫
genRandomMaze ::
  -- | start location
  Location ->
  -- | target location
  Location ->
  -- | how many rows
  Int ->
  -- | how many columns
  Int ->
  -- | how many barrier elements (percent, range 1-100)
  Int ->
  IO Maze
genRandomMaze start end row column sparseness = do
  rnd <- R.createSystemRandom
  m <- M.newArray ((0, 0), (row, column)) Empty :: IO (IOArray Index Cell)
  forM_ [(r, c) | r <- [0 .. row], c <- [0 .. column]] $ \loc -> do
    cell <-
      if
          | loc == start -> return Start
          | loc == end -> return Goal
          | otherwise -> genCell rnd sparseness
    M.writeArray m loc cell
  xs <- M.freeze m
  return $ Maze start end (Matrix xs)

genCell :: (Variate a, PrimMonad m, Num a, Ord a) => Gen (PrimState m) -> a -> m Cell
genCell g sparseness = do
  x <- uniformR (0, 100) g
  let c = if x <= sparseness then Barrier else Empty
  return c

genSquareMaze :: Int -> IO Maze
genSquareMaze size = let s = size - 1 in genRandomMaze (0, 0) (s, s) s s defaultSparseness

genSparseSquareMaze :: Int -> Int -> IO Maze
genSparseSquareMaze size sparseness = let s = size - 1 in genRandomMaze (0, 0) (s, s) s s sparseness

showMaze :: Maze -> IO ()
showMaze (Maze _start _end (Matrix arr)) = printGrid arr

printGrid :: Show a => Array (Int, Int) a -> IO ()
printGrid grid = mapM_ (putStrLn . textRepresentation) (toSimpleArray grid)

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid =
  let ((lowy, lowx), (highy, highx)) = bounds grid
   in [[grid ! (row, col) | col <- [lowx .. highx]] | row <- [lowy .. highy]]

textRepresentation :: Show a => [a] -> String
textRepresentation = foldl (\acc y -> acc ++ show y ++ " ") ""

successors :: Maze -> Index -> [Index]
successors (Maze _start _end (Matrix grid)) (x, y) =
  catMaybes
    [ if x + 1 <= highy && (grid ! (x + 1, y) /= Barrier) then Just (x + 1, y) else Nothing,
      if y + 1 <= highx && (grid ! (x, y + 1) /= Barrier) then Just (x, y + 1) else Nothing,
      if x - 1 >= lowy && (grid ! (x - 1, y) /= Barrier) then Just (x - 1, y) else Nothing,
      if y - 1 >= lowx && (grid ! (x, y - 1) /= Barrier) then Just (x, y - 1) else Nothing
    ]
  where
    ((lowy, lowx), (highy, highx)) = bounds grid

-- data Node a = Node {idx :: a, prev :: Maybe (Node a)} deriving (Show, Eq)
data Node a = Node {idx :: a, prev :: Maybe (Node a), cost :: Double, heuristic :: Double} deriving (Show, Eq)

type Path = Node Index

findPath :: (Maze -> Maybe Path) -> Maze -> [Index]
findPath search = flattenPath . search

flattenPath :: Maybe (Node a) -> [a]
flattenPath Nothing = []
flattenPath (Just node) = go node []
  where
    go (Node i Nothing _ _) xs = i : xs
    go (Node i (Just n) _ _) xs = go n (i : xs)

renderPath :: [Index] -> Maze -> Matrix
renderPath xs (Maze s e (Matrix grid)) = Matrix (markPath grid $ filter (\i -> i /= s && i /= e) xs)
  where
    markPath :: Array Index Cell -> [Index] -> Array Index Cell
    markPath grid path = grid // map (,Path) path

renderDfsPath :: Maze -> Matrix
renderDfsPath m = renderPath (findPath dfs m) m

renderBfsPath :: Maze -> Matrix
renderBfsPath m = renderPath (findPath bfs m) m

searchPath :: (Maze -> Maybe Path) -> Maze -> Matrix
searchPath alg m = renderPath (findPath alg m) m

-- 深度搜索
dfs :: Maze -> Maybe Path
dfs m@(Maze start end _) = go [Node start Nothing 0 0] Set.empty
  where
    go :: [Node Index] -> HashSet Index -> Maybe Path
    go [] _explored = Nothing
    go (x : xs) explored
      | idx x == end = Just x
      | otherwise = go (map (\i -> Node i (Just x) 0 0) unVisitedSuccessors ++ xs) (Set.insert currentIndex explored)
      where
        currentIndex = idx x
        unVisitedSuccessors = filter (not . flip Set.member explored) (successors m currentIndex)

-- 广度搜索
bfs :: Maze -> Maybe Path
bfs m@(Maze start end _) = go (Seq.singleton $ Node start Nothing 0 0) Set.empty
  where
    go :: Seq (Node Index) -> HashSet Index -> Maybe Path
    go Seq.Empty _explored = Nothing
    go (x :<| xs) explored
      | idx x == end = Just x
      | otherwise = go (xs >< unVisitedSuccessors) (Set.insert currentIndex explored)
      where
        currentIndex = idx x
        unVisitedSuccessors :: Seq (Node Index)
        unVisitedSuccessors =
          Seq.fromList
            . map (\i -> Node i (Just x) 0 0)
            . filter (not . flip Set.member explored)
            $ successors m currentIndex

euclideanDistance :: Location -> Location -> Double
euclideanDistance (x1, y1) (x2, y2) = sqrt . fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

manhattanDistance :: Location -> Location -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

astar ::
  -- | heuristic value function
  (Maze -> Location -> Double) ->
  -- | our maze
  Maze ->
  Maybe Path
astar heuristicFunc m@(Maze start end _) =
  go (PQ.singleton start (priority initialNode) initialNode) (Map.singleton start 0)
  where
    initialNode = Node start Nothing 0 (heuristicFunc m start)
    priority (Node _i _p c h) = c + h

    go :: HashPSQ Index Double (Node Index) -> HashMap Index Double -> Maybe Path
    go pending explored
      | null pending = Nothing
      | otherwise = case PQ.findMin pending of
        Just (i, _, n) ->
          if i == end
            then Just n
            else
              let !newCost = cost n + 1
                  !removeMin = PQ.deleteMin pending

                  !newUnVisitedNodes =
                    map (\l -> Node l (Just n) newCost (heuristicFunc m i))
                      . filter (maybe True (> newCost) . flip Map.lookup explored)
                      $ successors m i
                  newUnVisited =
                    foldl'
                      (\unVisited node -> PQ.insert (idx node) (priority node) node unVisited)
                      removeMin
                      newUnVisitedNodes
                  newExplored =
                    foldl'
                      (\costMap node -> Map.insert (idx node) newCost costMap)
                      explored
                      newUnVisitedNodes
               in go newUnVisited newExplored
        Nothing -> Nothing

manhattanDistanceHeuristicFunc :: Num a => Maze -> Location -> a
manhattanDistanceHeuristicFunc (Maze _ end _) = fromIntegral . manhattanDistance end

manhattonAstar :: Maze -> Maybe Path
manhattonAstar = astar manhattanDistanceHeuristicFunc

renderAstarPath :: Maze -> Matrix
renderAstarPath m = renderPath (findPath manhattonAstar m) m

-- |
-- m <- genSquareMaze 50
--
-- renderAstarPath m
-- renderDfsPath m
-- renderBfsPath m

-- | import Control.Monad (forM)
-- | m <- genRandomMaze (2,4) (4, 20) 5 30 15
-- | alg = [bfs, dfs, manhattonAstar]
-- | forM alg  (searchPath m)