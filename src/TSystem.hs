module TSystem where

import Common 

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


type LabelingFunction = Map.Map NodeIdent (Set.Set AtomIdent)
type TransitionFunction = Map.Map NodeIdent (Set.Set NodeIdent)

data TSystem = TSystem 
  { graph        :: Graph,
    initialNodes :: Set.Set NodeIdent,
    labeling     :: LabelingFunction
  }
  deriving (Eq, Show)

data Graph = Graph
  { nodes       :: Set.Set NodeIdent,
    transitions :: TransitionFunction
  }
  deriving (Eq, Show)



