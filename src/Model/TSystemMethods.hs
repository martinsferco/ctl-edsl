module Model.TSystemMethods where

import Model.TSystem
import MonadCTL
import Common

import qualified Data.Map as Map
import qualified Data.Set as Set
  

buildTSystem :: MonadCTL m => InfoNodes -> LabelingFunction -> m TSystem
buildTSystem infoNodes labeling = 
  do g <- buildGraph infoNodes
     nonBlockingGraph g
     nodeCoherence g labeling
     return $ TSystem g labeling

nodeCoherence :: MonadCTL m => Graph -> LabelingFunction -> m ()
nodeCoherence g l = if nodes g == Map.keysSet l then return ()
                                                else failCTL "incoherence"

getNodes :: TSystem -> Nodes
getNodes = nodes . graph

getInitialNodes :: TSystem -> Nodes
getInitialNodes = inits . graph

-- puede fallar si no exite el nodo
getNeighboors :: TSystem -> NodeIdent -> Nodes
getNeighboors ts node = ((.) trans graph ts) Map.! node 

-- puede fallar si no exite el nodo
getLabels :: TSystem -> NodeIdent -> Atoms
getLabels ts node = (labeling ts) Map.! node 

-- puede fallar si no existe el nodo
isLabel :: TSystem -> AtomIdent -> NodeIdent -> Bool
isLabel ts atom node = Set.member atom (getLabels ts node)



buildGraph :: MonadCTL m => InfoNodes -> m Graph
buildGraph (InfoNodes init trans) = return $ Graph allNodes init trans
  where allNodes = Set.union (Map.keysSet trans)
                             (Set.unions $ Map.elems trans) 

nonBlockingGraph :: MonadCTL m => Graph -> m ()
nonBlockingGraph graph = if nonBlocking then return ()
                                        else failCTL "blocking graph"
  where 
    nonBlocking = nodes graph == Map.keysSet (trans graph)

