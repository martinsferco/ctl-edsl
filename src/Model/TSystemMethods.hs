module Model.TSystemMethods where

import Model.TSystem
import MonadCTL
import Common

import qualified Data.Map as Map
import qualified Data.Set as Set
  

buildTSystem :: TransitionFunction -> LabelingFunction -> TSystem
buildTSystem = undefined

getNodes :: TSystem -> Nodes
getNodes = nodes . graph

getInitialNodes :: TSystem -> Nodes
getInitialNodes = initialNodes . graph

-- puede fallar si no exite el nodo
getNeighboors :: TSystem -> NodeIdent -> Nodes
getNeighboors ts node = ((.) transitions graph ts) Map.! node 

-- puede fallar si no exite el nodo
getLabels :: TSystem -> NodeIdent -> Atoms
getLabels ts node = (labeling ts) Map.! node 

-- puede fallar si no existe el nodo
isLabel :: TSystem -> AtomIdent -> NodeIdent -> Bool
isLabel ts atom node = Set.member atom (getLabels ts node)




buildGraph :: MonadCTL m => TransitionFunction -> m Graph
buildGraph tfun = undefined