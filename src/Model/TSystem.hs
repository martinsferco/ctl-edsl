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
    nodes :: Nodes,
    inits :: Nodes,
    trans :: TransitionFunction
  }
  deriving (Show, Eq)
