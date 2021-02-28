{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as Darr
import Data.Maybe

-- for moving pieces
data Directions = North | South | West | East | Other
    deriving (Show, Eq, Ord)

-- for storing pieces position (line, column)
type Position = (Int, Int)

-- pieces are represented by cells
data Cell = Cell
  { position :: Position
  , value :: Char
} deriving (Eq, Ord)

-- level represents a state (matrix of cells with their positions)
data Level = Level
  { cells :: (Darr.Array (Int, Int) Cell)
  , lin :: Int
  , col :: Int
  } deriving (Eq, Ord)

-- instance for printing the level (ascii characters used for every piece type)
instance Show Level 
    where
        -- print matrix of cells with foldl
        show (Level cells _ col) = (reverse getRepr) ++ "\n" where
            getRepr = foldl (\res (Cell position value) -> if ((snd position) /= 0)
                then (value : res)
                else (value : ('\n' : res)))
                    [] (Darr.elems cells)

-- creates a level with all cell locations empty 
emptyLevel :: Position -> Level
emptyLevel (lin, col) = (Level (buildMatrix lin col) lin col) where
    buildMatrix lin col = Darr.array ((0, 0), (lin, col)) [((i, j), (Cell (i, j) emptySpace)) | i <- [0..lin], j <- [0..col]]

-- add a piece to level (piece contains position and type)
addCell :: (Char, Position) -> Level -> Level
addCell newCell (Level cells lin col)
    -- check if position is valid (on map and empty cell at the location)
    | tempLin < 0 || tempLin > lin || tempCol < 0 || tempCol > col ||
        value (cells Darr.! (tempLin, tempCol)) /= emptySpace = level
    | otherwise = (Level (cells Darr.// [((tempLin, tempCol), (Cell (tempLin, tempCol) (fst newCell)))]) lin col)
        where
            level = (Level cells lin col)
            (tempLin, tempCol) = snd newCell

-- receive a pieces array and create a level (initial state) with foldl
createLevel :: Position -> [(Char, Position)] -> Level
createLevel position coords = foldl (\x res -> addCell res x) (emptyLevel position) coords

-- function for swapping 2 cells (update the 2 pieces position in matrix at the same time)
swapCells :: Position -> Position -> Level -> Level
swapCells src dst (Level cells lin col) = (Level newCells lin col)
    where
        cell_src = cells Darr.! src
        cell_dst = cells Darr.! dst
        newCells = cells Darr.// [(src, (Cell src (value cell_dst))), (dst, (Cell dst (value cell_src)))]

-- move a cell by swapping source with destination (after checking if move is valid), by direction
moveCell :: Position -> Directions -> Level -> Level
moveCell position direction (Level cells linBound colBound)
    | (direction == North
    && lin > 0
    && checkMove (lin - 1, col) == True)
        = swapCells (lin - 1, col) position level
    | (direction == South
    && lin < linBound
    && checkMove (lin + 1, col) == True)
        = swapCells (lin + 1, col) position level
    | (direction == West
    && col > 0
    && checkMove (lin, col - 1) == True)
        = swapCells (lin, col - 1) position level
    | (direction == East
    && col < colBound
    && checkMove (lin, col + 1) == True)
        = swapCells (lin, col + 1) position level
    | otherwise = (Level cells linBound colBound)
        where
            (lin, col) = position
            level = (Level cells linBound colBound)
            -- check if a piece can be moved
            checkMove nextPos =
                value (cells Darr.! nextPos) == emptySpace
                && tvalue `notElem` startCells
                && tvalue `notElem` winningCells
                    where tvalue = value (cells Darr.! position)

-- check if 2 pieces are connected, by direction
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell _ value1) (Cell _ value2) direction
    | (direction == North
    && value1 `elem` [verPipe, botLeft, botRight, startUp]
    && value2 `elem` [verPipe, topLeft, topRight, winDown])
        = True
    | (direction == South
    && value1 `elem` [verPipe, topLeft, topRight, startDown]
    && value2 `elem` [verPipe, botLeft, botRight, winUp])
        = True
    | (direction == West
    && value1 `elem` [horPipe, botRight, topRight, startLeft]
    && value2 `elem` [horPipe, topLeft, botLeft, winRight])
        = True
    | (direction == East
    && value1 `elem` [horPipe, topLeft, botLeft, startRight]
    && value2 `elem` [horPipe, botRight, topRight, winLeft])
        = True
    | otherwise = False

-- check if a level is solved (if there is road between start and end)
wonLevel :: Level -> Bool
wonLevel (Level cells linBound colBound) = wonHelper cells (findStart (Darr.elems cells)) Other
    where
        -- find start cell in level
        findStart cells
            | ((value (head cells)) `elem` startCells) = (head cells)
            | otherwise = findStart (tail cells)
        -- receive come direction (not to turn back) and check if there is a connection forward
        wonHelper allCells (Cell position value) comeDir
            | (value `elem` winningCells)
                = True
            | (comeDir /= North
            && lin > 0
            && connection (Cell position value) (allCells Darr.! (lin - 1, col)) North == True)
                = wonHelper allCells (allCells Darr.! (lin - 1, col)) South
            | (comeDir /= South
            && lin < linBound
            && connection (Cell position value) (allCells Darr.! (lin + 1, col)) South == True)
                = wonHelper allCells (allCells Darr.! (lin + 1, col)) North
            | (comeDir /= West
            && col > 0
            && connection (Cell position value) (allCells Darr.! (lin, col - 1)) West == True)
                = wonHelper allCells (allCells Darr.! (lin, col - 1)) East
            | (comeDir /= East
            && col < colBound
            && connection (Cell position value) (allCells Darr.! (lin, col + 1)) East == True)
            = wonHelper allCells (allCells Darr.! (lin, col + 1)) West
            | otherwise = False
                where
                    (lin, col) = position


instance ProblemState Level (Position, Directions) where
    -- get all successors for a state (succesor = (action == (position, direction), nextState))
    successors (Level cells linBound colBound) = foldl (\res cell -> (generateStates level cell res)) [] (Darr.elems cells)
        where
            level = (Level cells linBound colBound)
            -- generate all next states from all directions for a state
            generateStates level cell res = res ++ checkNorth ++ checkSouth ++ checkWest ++ checkEast
                where
                    checkNorth
                        | (lin == 0 || checkNotValid (lin - 1, col) == True) = []
                        | otherwise = [((tposition, North), swapCells tposition (lin - 1, col) level)]
                    checkSouth
                        | (lin == linBound || checkNotValid (lin + 1, col) == True) = []
                        | otherwise = [((tposition, South), swapCells tposition (lin + 1, col) level)]
                    checkWest
                        | (col == 0 || checkNotValid (lin, col - 1) == True) = []
                        | otherwise = [((tposition, West), swapCells tposition (lin, col - 1) level)]
                    checkEast
                        | (col == colBound || checkNotValid (lin, col + 1) == True) = []
                        | otherwise = [((tposition, East), swapCells tposition (lin, col + 1) level)]
                    -- check if the move (action) is valid
                    checkNotValid position =
                        value (cells Darr.! position) /= emptySpace
                        || tvalue `elem` startCells
                        || tvalue `elem` winningCells
                        || tvalue == emptySpace
                    tposition = position cell
                    (lin, col) = tposition
                    tvalue = value cell

    -- state is final <=> level is won
    isGoal = wonLevel

    -- return the previous state for a given action
    reverseAction (((lin, col), direction), level)
        | direction == North
            = (((lin - 1, col), South), level)
        | direction == South
            = (((lin + 1, col), North), level)
        | direction == West
            = (((lin, col - 1), East), level)
        | direction == East
            = (((lin, col + 1), West), level)
        