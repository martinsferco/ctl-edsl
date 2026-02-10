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
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)


buildTSystem :: MonadCTL m => InfoNodes -> LabelingFunction -> m TSystem
buildTSystem infoNodes lab = 
  do g <- buildGraph infoNodes
     nonBlockingGraph g
     labelCoherence g lab
     return $ TSystem g (completeLabeling g lab)

labelCoherence :: MonadCTL m => Graph -> LabelingFunction -> m ()
labelCoherence modelGraph labels = unless (Map.keysSet labels `Set.isSubsetOf` nodes modelGraph)
                                          (failCTL "Labeling function is refering to a node not defined in the graph")

completeLabeling :: Graph -> LabelingFunction -> LabelingFunction
completeLabeling modelGraph labels = 
  let allNodes = nodes modelGraph
      labeledNodes = Map.keysSet labels
      notLabeledNodes = Set.difference allNodes labeledNodes
  in Map.union (Map.fromSet (const Set.empty) notLabeledNodes) labels

getNodes :: TSystem -> Nodes
getNodes = nodes . graph

setInitialNodes :: TSystem -> Nodes -> TSystem
setInitialNodes ts newNodes = ts { graph = (graph ts) { inits = newNodes } }

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
buildGraph (InfoNodes initNodes modelTransitions) = return $ Graph allNodes initNodes modelTransitions
  where allNodes = Set.union (Map.keysSet modelTransitions)
                             (Set.unions $ Map.elems modelTransitions) 

nonBlockingGraph :: MonadCTL m => Graph -> m ()
nonBlockingGraph modelGraph = if nonBlocking then return ()
                                        else failCTL "trying to define a blocking graph"
  where 
    nonBlocking = nodes modelGraph == Map.keysSet (trans modelGraph)

findCycleInSubgraph :: MonadCTL m => TSystem -> Nodes -> NodeIdent -> m (Maybe [NodeIdent])
findCycleInSubgraph ts validNodes start = findCycleInSubgraph' [] start
  where
    findCycleInSubgraph' :: MonadCTL m => [NodeIdent] -> NodeIdent -> m (Maybe [NodeIdent])
    findCycleInSubgraph' path current
      | current `elem` path                = return (Just $ reverse (current : path))
      | current `Set.notMember` validNodes = return Nothing
      | otherwise = do  
          let path' = current : path
          nexts <- getNeighboors ts current
          let validNexts = filter (`Set.member` validNodes) (Set.toList nexts)
          tryEach [findCycleInSubgraph' path' next | next <- validNexts]

findPathToTargetInSubgraph :: MonadCTL m => TSystem -> Nodes -> NodeIdent -> Nodes -> m (Maybe [NodeIdent])
findPathToTargetInSubgraph ts validPathNodes start targetNodes = findPathToTargetInSubgraph' [] start
  where
    findPathToTargetInSubgraph' :: MonadCTL m => [NodeIdent] -> NodeIdent -> m (Maybe [NodeIdent])
    findPathToTargetInSubgraph' path current
      | current `Set.member` targetNodes       = return (Just $ reverse (current : path))
      | current `elem` path                    = return Nothing
      | current `Set.notMember` validPathNodes = return Nothing          
      | otherwise = do
          let path' = current : path
          nexts <- getNeighboors ts current
          let validNodes = Set.union validPathNodes targetNodes
          let validNexts = filter (`Set.member` validNodes) (Set.toList nexts)
          tryEach [findPathToTargetInSubgraph' path' next | next <- validNexts]

tryEach :: MonadCTL m => [m (Maybe a)] -> m (Maybe a)
tryEach [] = return Nothing
tryEach (action : rest) = do
  result <- action
  case result of
    Just x  -> return (Just x)
    Nothing -> tryEach rest

exportTSystem :: MonadCTL m => TSystem -> String -> m ()
exportTSystem ts fileName = 
  do exportedGraph <- buildExportedTSystem ts
     _ <- liftIO $ runGraphviz exportedGraph Pdf (exportFolder ++ fileName ++ fileExtension)
     printCTL "[] Transition system exported !"
  where 
    fileExtension = ".pdf"
    exportFolder = "export/"


buildExportedTSystem :: MonadCTL m => TSystem -> m (DotGraph NodeIdent)
buildExportedTSystem ts = 
  do  exportesNodes <- buildExportedNodes ts
      exportedEdges <- buildExportedEdges ts
      return $ graphElemsToDot paramsExport exportesNodes exportedEdges
      
  where 
    paramsExport = nonClusteredParams
      { fmtNode = \(_, (label, initial)) -> [ Label (StrLabel $ pack label),
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
buildLabel label node = node ++ ": {" ++ intercalate "," (Set.toList $ label Map.! node) ++ "}"