module Model.TSystemMethods where

import Model.TSystem
import MonadCTL
import Common

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( intercalate )

import Control.Monad.IO.Class
import Control.Monad (unless)

import Data.GraphViz
import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)


buildTSystem :: MonadCTL m => InfoNodes -> LabelingFunction -> m TSystem
buildTSystem infoNodes labeling = 
  do g <- buildGraph infoNodes
     nonBlockingGraph g
     labelCoherence g labeling
     return $ TSystem g (completeLabeling g labeling)

labelCoherence :: MonadCTL m => Graph -> LabelingFunction -> m ()
labelCoherence graph labels = unless (Map.keysSet labels `Set.isSubsetOf` nodes graph)
                                      (failCTL "a labeling function is refering to a node not defined in the graph")

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
                           (failCTL $ (show node) ++ " is not a node in the transition system")

buildGraph :: MonadCTL m => InfoNodes -> m Graph
buildGraph (InfoNodes init trans) = return $ Graph allNodes init trans
  where allNodes = Set.union (Map.keysSet trans)
                             (Set.unions $ Map.elems trans) 

nonBlockingGraph :: MonadCTL m => Graph -> m ()
nonBlockingGraph graph = if nonBlocking then return ()
                                        else failCTL "trying to define a blocking graph"
  where 
    nonBlocking = nodes graph == Map.keysSet (trans graph)



exportTSystem :: MonadCTL m => TSystem -> String -> m ()
exportTSystem ts fileName = 
  do exportedGraph <- buildExportedTSystem ts
     liftIO $ runGraphviz exportedGraph Pdf (fileName ++ fileExtension)
     printCTL "Grafo exportado!"
  where 
    fileExtension = ".pdf"


buildExportedTSystem :: MonadCTL m => TSystem -> m (DotGraph NodeIdent)
buildExportedTSystem ts = 
  do  exportesNodes <- buildExportedNodes ts
      exportedEdges <- buildExportedEdges ts
      return $ graphElemsToDot paramsExport exportesNodes exportedEdges
      
  where 
    paramsExport = nonClusteredParams
      { fmtNode = \(n, (label, initial)) -> [ Label (StrLabel $ pack label),
                                              Shape (if initial then DoubleCircle else Circle) ]
      , fmtEdge = const []
      }

type ExportedNode = (NodeIdent, (String, Bool))
type ExportedEdge = (NodeIdent, NodeIdent, ())

buildExportedNodes :: MonadCTL m => TSystem -> m [ExportedNode]
buildExportedNodes ts = return $ map buildNode (Set.toList $ getNodes ts)
  where buildNode :: NodeIdent -> ExportedNode
        buildNode node = let initial = node `Set.member` (getInitialNodes ts)
                             label = buildLabel (labeling ts) node
                         in (node, (label, initial))

buildExportedEdges :: MonadCTL m => TSystem -> m [ExportedEdge]
buildExportedEdges ts = concat <$> mapM buildTransitions (Set.toList $ getNodes ts) 
  where buildTransitions :: MonadCTL m => NodeIdent -> m [ExportedEdge]
        buildTransitions node = map (\n -> (node, n, ())) <$> (Set.toList <$> getNeighboors ts node)


buildLabel :: LabelingFunction -> NodeIdent -> String
buildLabel labeling node = intercalate "," (Set.toList $ labeling Map.! node)

