{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import ProblemState
import Data.Maybe

-- data structure for storing states space
data Node s a = Node
  { state :: s
  , action :: (Maybe a)
  , parent :: (Maybe (Node s a))
  , depth :: Int
  , children :: [(Node s a)]
  } deriving (Eq, Ord, Show)

-- constructors (not used)
nodeState :: Node s a -> s
nodeState (Node state _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _  parent _ _) = parent

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ depth _) = depth

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ action _ _ _) = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ children) = children

-- create the initial state for problem (lazy programming) --- a node's children are his state' succesors
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace state = newNode
    where
        -- create first node (root)
        newNode = (Node state Nothing Nothing 0 (map (\x -> (generateHelper newNode (snd x) (fst x))) (successors state)))
            where
                -- create next node by actual node (parent)
                generateHelper parent state action = newNode
                    where
                        newNode = (Node state (Just action) (Just parent) (depth parent + 1)
                            (map (\x -> (generateHelper newNode (snd x) (fst x))) (successors state)))

-- bfs traversal in states space tree
-- first elem of pair is a list of children for the current node
-- second elem of pair is the queue where current node is extracted from
-- visited for not go through a state twice
bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = bfsHelper [([node], [node])] []
    where
        bfsHelper res visited
            | ((checkVisited (head queue) visited) == True) = res ++ (bfsHelper [([], tail queue)] visited)
            | otherwise = res ++ (bfsHelper [(childrens, (tail queue) ++ childrens)] (visited ++ [head queue]))
                where
                    queue = (snd (head res))
                    childrens = (children (head (snd (head res))))
                    checkVisited node visited = ((state node) `elem` (map (\x -> state x) visited))

-- make bidirectional bfs, one from first state, one from final state, to get the path between them
bidirBFS :: (Ord s, Eq a) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS node1 node2 = foldr (\pair res -> (updateRes res pair)) (node1, node2) (zip (bfs node1) (bfs node2))
    where
        -- update common node if found
        updateRes res pair
            | (newRes /= []) = (head newRes)
            | otherwise = res
            where
                -- find common node between initial and final state
                newRes = (commonElem (snd (fst pair)) (fst (snd pair)))
                    where
                        -- check if 2 lists have a common elem --- returns a list with it if found
                        commonElem list1 list2
                            | (list1 == []) = []
                            | ((existElem (head list1) list2) /= []) = (existElem (head list1) list2)
                            | otherwise = (commonElem (tail list1) list2)
                                where
                                    -- check if an elem is in a list, return a list with it if found
                                    existElem x list
                                        | (list == []) = []
                                        | ((state x) == (state (head list))) = [(x, head list)]
                                        | otherwise = (existElem x (tail list))

-- given a node, extract the path from node to his root (in reverse order)
extractPath ::(Eq s, Eq a) => Node s a -> [(Maybe a, s)]
extractPath node = extractHelper node []
    where
        extractHelper node res
            | ((parent node) == Nothing) = ([(Nothing, state node)] ++ res)
            | otherwise = (extractHelper (fromJust (parent node)) ([(action node, state node)] ++ res))

-- solve problem (make bidirectional bfs on initial and final states, extract path and rebuild moves)
solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- initial state
      -> s          -- final state
      -> [(Maybe a, s)]   -- moves list
solve start end = (extractPath start1) ++ (obtainList (reverse (extractPath end1)))
    where
        (start1, end1) = bidirBFS (createStateSpace start) (createStateSpace end)
        obtainList list = solveHelper (head list) (tail list) []
            where
                -- reverse all actions that lead from final state to 'common node' in bfs
                solveHelper x list res
                    | (list == []) = res
                    | otherwise = solveHelper (head list) (tail list) (res ++ [(Just (fst reversed), snd reversed)])
                        where reversed = reverseAction (fromJust (fst x), snd (head list))
