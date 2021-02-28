{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleContexts, InstanceSigs #-}

module AStarHeuristic where
import RollTheBall
import ProblemState
import Search

import Data.Hashable
import Data.Graph.AStar
import Data.Char
import Data.Array as Darr
import qualified Data.HashSet as H

levelToList :: Level -> [Cell]
levelToList (Level cells _ _)= Darr.elems cells

-- hash function for a cell
instance Hashable Cell where
	hashWithSalt :: Int -> Cell -> Int
	hashWithSalt coef (Cell (lin, col) value) = (coef * lin) + (col * ord (value))

-- hash function for a level
instance Hashable Level where
	hashWithSalt :: Int -> Level -> Int
	hashWithSalt coef (Level cells lin col) = foldl (\res x -> res + lin * (hashWithSalt coef x)) (lin * col) (Darr.elems cells)

-- make a HashSet from all state successors
neighbours :: (Level -> H.HashSet Level)
neighbours level = H.fromList (getNextLevels level)
	where
		getNextLevels level = map snd (successors level)

-- distance between 2 diacent levels in states space (nodes)
distance :: (Num c) => (Level -> Level -> c)
distance level1 level2 = 1

-- constant heuristic value for checking AStar algorithm
trivialHeuristic :: (Num a) => Level -> a
trivialHeuristic _ = 10

-- non constant heuristic used by AStar algorithm
nonTrivialHeuristic :: (Num c) => Level -> c
nonTrivialHeuristic level = value
	where
		(list1, value, list2) = (nonTrivialHelper ([level], 0, []))
		-- make bfs starting from given value until the level is won
		nonTrivialHelper (levels, value, visited)
			| (checkWon levels == True) = ([], value, [])
			| otherwise = nonTrivialHelper (getAllChildren levels, value + 1, visited ++ visitedLevels)
				where
					-- get all non visited states
					visitedLevels = filterVisited levels visited
						where
							filterVisited levels visited = filter (\x -> (x `notElem` visited)) levels
					-- check if a state is final
					checkWon levels
						| (levels == []) = False
						| (isGoalNode (head levels) == True) = True
						| otherwise = (checkWon (tail levels))
					-- similar into successors function, get all next states
					getAllChildren levels = foldl (\y res -> res ++ y) [] (map (\x -> getNextLevels x) levels)
						where
							getNextLevels level = map snd (successors level)

-- goal node <=> level finished
isGoalNode :: Level -> Bool
isGoalNode = wonLevel
