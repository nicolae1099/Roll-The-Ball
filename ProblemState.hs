{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

 
    -- exposes the functions necessary to build the space of states, for a problem

    --  `s` and` a` represent the types of states, respectively the actions that transform one state into another

    -- the `s -> a` syntax in the class header means that `s` uniquely determines `a`

class ProblemState s a | s -> a where

    -- for the current state, provides the list of pairs (action, next state)
    successors :: s -> [(a, s)]

    -- returns `True` if the current state is final
    isGoal :: s -> Bool

    -- reverse the action
    -- eg: (South, (1, 0)) -> (North, (2, 0))
    reverseAction :: (a, s) -> (a, s)

