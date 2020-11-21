#! /usr/bin/env bash
-- stack --resolver lts script --package data-structures 

import Maze
import Data.Traversable

main:: IO ()
main = do
  result <- forM [bfs, dfs, manhattanAstar] searchPath <$> genRandomMaze (2,4) (4, 20) 5 30 20
  print result