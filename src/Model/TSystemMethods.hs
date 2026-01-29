module Model.TSystemMethods where

import Model.TSystem
import MonadCTL
import Common

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (unless)

buildTSystem :: MonadCTL m => InfoNodes -> LabelingFunction -> m TSystem
buildTSystem infoNodes labeling = 
  do g <- buildGraph infoNodes
     nonBlockingGraph g
     labelCoherence g labeling
     return $ TSystem g (completeLabeling g labeling)

labelCoherence :: MonadCTL m => Graph -> LabelingFunction -> m ()
labelCoherence graph labels = unless (Map.keysSet labels `Set.isSubsetOf` nodes graph)
                                      (modelError "a labeling function is refering to a node not defined in the graph")

completeLabeling :: Graph -> LabelingFunction -> LabelingFunction
completeLabeling graph labels = 
  let allNodes = nodes graph
      labeledNodes = Map.keysSet labels
      notLabeledNodes = Set.difference allNodes labeledNodes
  in Map.union (Map.fromSet (const Set.empty) notLabeledNodes) labels

getNodes :: TSystem -> Nodes
getNodes = nodes . graph

getInitialNodes :: TSystem -> Nodes
getInitialNodes = inits . graph

getNeighboors :: MonadCTL m => TSystem -> NodeIdent -> m Nodes
getNeighboors ts node = node `isNodeOf` ts >>
                        (return $ ((.) trans graph ts) Map.! node)
                       
getLabels :: MonadCTL m => TSystem -> NodeIdent -> m Atoms
getLabels ts node = node `isNodeOf` ts >>
                    (return $ (labeling ts) Map.! node)

isLabel :: MonadCTL m => TSystem -> AtomIdent -> NodeIdent -> m Bool
isLabel ts atom node =  node `isNodeOf` ts >>
                        (Set.member atom <$> getLabels ts node)

isNodeOf :: MonadCTL m => NodeIdent -> TSystem -> m ()
isNodeOf node ts =  unless (node `Set.member` (getNodes ts))
                           (modelError $ (show node) ++ " is not a node in the transition system")

buildGraph :: MonadCTL m => InfoNodes -> m Graph
buildGraph (InfoNodes init trans) = return $ Graph allNodes init trans
  where allNodes = Set.union (Map.keysSet trans)
                             (Set.unions $ Map.elems trans) 

nonBlockingGraph :: MonadCTL m => Graph -> m ()
nonBlockingGraph graph = if nonBlocking then return ()
                                        else failCTL "blocking graph"
  where 
    nonBlocking = nodes graph == Map.keysSet (trans graph)



exportTSystem :: MonadCTL m => TSystem -> m ()
exportTSystem ts = return ()