module Model.TSystem where

import Common

data TSystem = TSystem
  {
    graph    :: Graph,
    labeling :: LabelingFunction
  }
  deriving (Show, Eq)


data Graph = Graph
  {
    nodes        :: Nodes,
    initialNodes :: Nodes,
    transitions  :: TransitionFunction
  }
  deriving (Show, Eq)
