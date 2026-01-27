module TSystem where

import Common 

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


type LabelingFunction = Map.Map NodeIdent Atoms
type TransitionFunction = Map.Map NodeIdent Nodes

data TSystem = TSystem 
  { graph        :: Graph,
    initialNodes :: Nodes,
    labeling     :: LabelingFunction
  }
  deriving (Eq, Show)

data Graph = Graph
  { nodes       :: Nodes,
    transitions :: TransitionFunction
  }
  deriving (Eq, Show)


getNodes :: TSystem -> Nodes
getNodes = nodes . graph

getInitialNodes :: TSystem -> Nodes
getInitialNodes = initialNodes

-- puede fallar si no exite el nodo
getNeighboors :: TSystem -> NodeIdent -> Nodes
getNeighboors ts node = ((.) transitions graph ts) Map.! node 

-- puede fallar si no exite el nodo
getLabels :: TSystem -> NodeIdent -> Atoms
getLabels ts node = (labeling ts) Map.! node 

isLabel :: TSystem -> AtomIdent -> NodeIdent -> Bool
isLabel ts atom node = Set.member atom (getLabels ts node)