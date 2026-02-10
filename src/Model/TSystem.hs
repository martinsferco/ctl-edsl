module Model.TSystem
  ( TSystem (..)
  , Graph (..) ) where

import Common ( LabelingFunction, TransitionFunction, Nodes )

data TSystem = TSystem
  {
    graph    :: Graph,            -- Graph asociated with the Transition System 
    labeling :: LabelingFunction  -- Labeling of each node of the graph.
  }
  deriving (Show, Eq)


data Graph = Graph
  {
    nodes :: Nodes,              -- All nodes of the graph.
    inits :: Nodes,              -- Initial nodes
    trans :: TransitionFunction  -- Transitions
  }
  deriving (Show, Eq)