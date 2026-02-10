module Sat where

import Model.TSystemMethods
import PrettyPrinter
import Model.TSystem
import EvalResult
import MonadCTL
import Common 
import Lang

import Control.Monad ( filterM, join )

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (subsequences)


models :: MonadCTL m => TSystem -> Formula -> m EvalResult
models ts form = do satForm <- sat ts form
                    let initNodes = getInitialNodes ts
                    let checkModels = initNodes `Set.isSubsetOf` satForm
                    
                    example <- if initNodes == Set.empty
                               then return []
                               else do
                                  initialNode <- choice initNodes
                                  if checkModels 
                                  then witness initialNode ts form
                                  else counterExample initialNode ts form

                    return $ EvalResult 
                           { evalFormula = form
                           , holds       = checkModels 
                           , satNodes    = satForm
                           , checkType   = CheckInitials initNodes
                           , examplePath = example }


isValid :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m EvalResult
isValid ts node form = do satForm <- sat ts form
                          let checkValid = node `Set.member` satForm
                          example <- if checkValid then witness node ts form
                                                   else counterExample node ts form
                          return $ EvalResult 
                                 { evalFormula = form
                                 , holds       = checkValid
                                 , satNodes    = satForm
                                 , checkType   = CheckNode node
                                 , examplePath = example }


isSatis :: MonadCTL m => Formula -> m (Maybe TSystem)
isSatis form = isSatis' (tSystems $ getAtoms form) form

  where isSatis' :: MonadCTL m => [TSystem] -> Formula -> m (Maybe TSystem)
        isSatis' []       _ = return Nothing
        isSatis' (ts:tss) f = do satF <- sat ts f
                                 if satF /= Set.empty then return $ Just (setInitialNodes ts satF)
                                                          else isSatis' tss f


sat :: MonadCTL m => TSystem -> Formula -> m Nodes
sat ts = (sat' ts) . transform
  where 
    sat' :: MonadCTL m => TSystem -> Formula -> m Nodes
    sat' _  F                   = return Set.empty
    sat' m  T                   = return $ getNodes m
    sat' m (Atom a)             = setFilterM (isLabel m a) (getNodes m)
    sat' m (Not p)              = Set.difference (getNodes m) <$> (sat' m p)
    sat' m (BinaryOp And p q)   = Set.intersection <$> (sat' m p) <*> (sat' m q)
    sat' m (UQuantifier EC p)   = sat' m p >>= preExist m
    sat' m (UQuantifier AC p)   = sat' m p >>= preForAll m
    sat' m (UQuantifier AR p)   = sat' m p >>= inev m
    sat' m (BQuantifier EU p q) = join (exUntil m <$> sat' m p <*> sat' m q)
    sat' _ f                    = failCTL $ "The sub-formula " ++ show f ++ " should be already trasnformed"


transform :: Formula -> Formula
transform F                      = F
transform T                      = T
transform a@(Atom _)             = a
transform (Not p)                = Not (transform p)
transform (BinaryOp And p q)     = BinaryOp And (transform p) (transform q)

transform (BinaryOp Or p q)      =  let tp = Not (transform p)
                                        tq = Not (transform q)
                                    in Not (BinaryOp And tp tq)
  
transform (BinaryOp Implies p q) =  let tp = transform p
                                        tq = Not (transform q)
                                    in Not (BinaryOp And tp tq)
  
transform (UQuantifier AC p)     = UQuantifier AC $ transform p
transform (UQuantifier EC p)     = UQuantifier EC $ transform p
transform (UQuantifier AR p)     = UQuantifier AR $ transform p
transform (UQuantifier ER p)     = BQuantifier EU T $ transform p
transform (UQuantifier AS p)     = Not (BQuantifier EU T (Not $ transform p))
transform (UQuantifier ES p)     = Not (UQuantifier AR (Not $ transform p))

transform (BQuantifier AU p q)   =  let tp = transform p
                                        tq = transform q
                                        neitherPQ = BinaryOp And (Not tp) (Not tq)
                                        untilPath = Not (BQuantifier EU (Not tq) neitherPQ) 
                                        inevitableQ = UQuantifier AR $ transform q
                                    in BinaryOp And untilPath inevitableQ

transform (BQuantifier EU p q)   = BQuantifier EU (transform p) (transform q)


inev :: MonadCTL m => TSystem -> Nodes -> m Nodes
inev ts x = do x' <- (Set.union x <$> (preForAll ts x))
               if x' == x then return x
                          else inev ts x'

exUntil :: MonadCTL m => TSystem -> Nodes -> Nodes -> m Nodes
exUntil ts x y  = do y' <- Set.union y <$> (Set.intersection x <$> (preExist ts y))
                     if y' == y then return y  
                                else exUntil ts x y'
                                
preExist :: MonadCTL m => TSystem -> Nodes -> m Nodes
preExist ts x = setFilterM existS (getNodes ts)
  where
    existS :: MonadCTL m => NodeIdent -> m Bool
    existS node = not <$> (Set.disjoint <$> (getNeighboors ts node) <*> pure x)

preForAll :: MonadCTL m => TSystem -> Nodes -> m Nodes
preForAll ts x = setFilterM forAllS (getNodes ts)
  where 
    forAllS :: MonadCTL m => NodeIdent -> m Bool
    forAllS node = Set.isSubsetOf <$> (getNeighboors ts node) <*> pure x


getAtoms :: Formula -> Atoms
getAtoms F                   = Set.empty 
getAtoms T                   = Set.empty
getAtoms (Atom atom)         = Set.singleton atom
getAtoms (Not p)             = getAtoms p
getAtoms (BinaryOp _ p q)    = Set.union (getAtoms p) (getAtoms q)
getAtoms (UQuantifier _ p)   = getAtoms p
getAtoms (BQuantifier _ p q) = Set.union (getAtoms p) (getAtoms q)


tSystems :: Atoms -> [TSystem]
tSystems atoms = concat $ map (\k -> tSystemGenerator atoms k) [1..]

allLabelings :: Nodes -> Atoms -> [LabelingFunction]
allLabelings nodeSet atoms = allCombinations nodeSet atoms

allTransitions :: Nodes -> [TransitionFunction]
allTransitions nodeSet = allCombinations nodeSet nodeSet

allCombinations :: Ord t => Nodes -> Set.Set t ->[Map.Map NodeIdent (Set.Set t)]
allCombinations nodeSet values =
  let nodeList = Set.toList nodeSet
      valueSubs = allSubsets values
      choices  = sequence (replicate (length nodeList) valueSubs)
  in map (Map.fromList . zip nodeList) choices

  where allSubsets :: Ord a => Set.Set a -> [Set.Set a]
        allSubsets s = map Set.fromList (subsequences (Set.toList s))


tSystemGenerator :: Atoms -> Int -> [TSystem]
tSystemGenerator atoms size =
  let nodesSet    = Set.fromList (map (\n -> "_n" ++ show n) [1 .. size])
      initials    = Set.empty
      everyLabeling   = allLabelings nodesSet atoms
      everyTransition = allTransitions nodesSet
  in [ TSystem { graph = Graph nodesSet initials transF, labeling = labF }
     | transF   <- everyTransition
     , labF     <- everyLabeling ]

setFilterM :: (Ord a, MonadCTL m) => (a -> m Bool) -> Set.Set a -> m (Set.Set a)
setFilterM p = fmap Set.fromList . filterM p . Set.toList
  



counterExample :: MonadCTL m => NodeIdent -> TSystem -> Formula -> m ExamplePath
counterExample node _  F                      = return [(node, modelsText False node F)]
counterExample _    _  T                      = failCTL "It is impossible to find a counterexample for ⊤!"
counterExample node _  (Atom atom )           = return [(node, atom ++ " is not an element of the labels of " ++ node)] 
counterExample node ts (Not p)                = witness node ts p  
counterExample node ts (BinaryOp And p q)     = do satP <- sat ts p
                                                   let form = if node `Set.member` satP then q else p
                                                   return [(node, modelsText False node form)]

counterExample node _  (BinaryOp Or p q)      = return [(node, modelsText False node p ++ ", " ++ modelsText False node q)]
counterExample node _  (BinaryOp Implies p q) = return [(node, modelsText True  node p ++ ", " ++ modelsText False node q)]

counterExample node ts (UQuantifier AC p)     = do satP <- sat ts p
                                                   neighs <- getNeighboors ts node
                                                   badNeigh <- choice (neighs `Set.difference` satP)
                                                   return [ (node, "Starting node"),(badNeigh, modelsText False badNeigh p)]

counterExample node _  (UQuantifier EC p)     = return [(node, "All neighboors of " ++ node ++ " do not satisfy " ++ ppFormula p)]         
counterExample node ts (UQuantifier AR p)     = findCycleOutsideSatFormula ts node p 
counterExample node _  (UQuantifier ER p)     = return [(node, "All traces starting at " ++ node ++ ", never reach an state that satifies " ++ ppFormula p)]         
counterExample node ts (UQuantifier AS p)     = findPathOutsideSatFormula ts node p
counterExample node _  (UQuantifier ES p)     = return [(node, "All traces starting at " ++ node ++ ", reach an state that does not satisfies " ++ ppFormula p)]         
counterExample node ts (BQuantifier AU p q)   = findForallUntilCounterExample ts node p q
counterExample node _  (BQuantifier EU _ _)   = return [(node, "All traces starting at " ++ node ++ ", do not satify the semantic of the Until operator")]         




findCycleOutsideSatFormula :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ExamplePath
findCycleOutsideSatFormula ts start form = do
  let notFormula = Not form
  satNotFormula <- sat ts notFormula
  maybeCicle <- findCycleInSubgraph ts satNotFormula start 
  case maybeCicle of
    Just cycleExample -> return $ addCycleDescription notFormula cycleExample
    Nothing           -> failCTL "No cycle found in the set of nodes in which the the formula is not satisfiable"
    
findPathOutsideSatFormula :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ExamplePath
findPathOutsideSatFormula ts start form = do
  let notFormula = Not form
  satNotFormula <- sat ts notFormula
  maybePath <- findPathToTargetInSubgraph ts (getNodes ts) start satNotFormula
  case maybePath of
    Just path -> return $ addPathDescription form notFormula path
    Nothing -> failCTL "No path found that reaches a set of nodes that do not satisfy the formula."
 
findCycleInSatFormula :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ExamplePath
findCycleInSatFormula ts start form = do
  satFormula <- sat ts form
  maybeCicle <- findCycleInSubgraph ts satFormula start 
  case maybeCicle of
    Just cycleExample -> return $ addCycleDescription form cycleExample
    Nothing           -> failCTL "No cycle found inside the nodes in which the formula is satisfiable"
    
findPathToSatFormula :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ExamplePath
findPathToSatFormula ts start form = do
  satFormula <- sat ts form
  maybePath <- findPathToTargetInSubgraph ts (getNodes ts) start satFormula
  case maybePath of
    Just path -> return $ addPathDescription (Not form) form path
    Nothing -> failCTL "No path found that reaches a set of nodes do satisfy the formula."

findForallUntilCounterExample :: MonadCTL m => TSystem -> NodeIdent -> Formula -> Formula -> m ExamplePath
findForallUntilCounterExample ts start p q = do 
  let pNotQ = BinaryOp And p (Not q)
  let notPNotQ = BinaryOp And (Not p) (Not q)
  satPNotQ <- sat ts pNotQ 
  satNotPNotQ <- sat ts notPNotQ
  maybeCycle  <- findCycleInSubgraph ts satPNotQ start
  case maybeCycle of
   Just cycleExample -> return $ addCycleDescription pNotQ cycleExample
   Nothing           -> do
     maybePath <- findPathToTargetInSubgraph ts satPNotQ start satNotPNotQ
     case maybePath of
       Just path -> return $ addPathDescription pNotQ notPNotQ path
       Nothing -> failCTL "No counterexample found for the forall until operator."


findExistUntilWitness :: MonadCTL m => TSystem -> NodeIdent -> Formula -> Formula -> m ExamplePath
findExistUntilWitness ts start p q = do 
  satP <- sat ts p 
  satQ <- sat ts q
  maybePath <- findPathToTargetInSubgraph ts satP start satQ
  case maybePath of
    Just path -> return $ addPathDescription p q path
    Nothing -> failCTL "No witness found for the exist until operator."


addCycleDescription :: Formula -> [NodeIdent] -> ExamplePath
addCycleDescription form c = map (\n -> (n, modelsText True n form)) (init c) ++ 
                                    [(last c, modelsText True (last c) form ++ " - Cycle detection")]

addPathDescription :: Formula -> Formula -> [NodeIdent] -> ExamplePath
addPathDescription form endFormula path = map (\n -> (n, modelsText True n form)) (init path) ++ 
                                             [(last path, modelsText True (last path) endFormula)]

modelsText :: Bool -> NodeIdent -> Formula -> String
modelsText isModel node form = "M," ++ node ++ " " ++ symbol ++ ppFormula form
  where symbol = if isModel then "⊨ " else "⊭ "


witness :: MonadCTL m => NodeIdent -> TSystem -> Formula -> m ExamplePath
witness _    _  F                      = failCTL "It is impossible to find a witness for ⊥!"
witness node _  T                      = return [(node, modelsText True node T)]
witness node _  (Atom atom )           = return [(node, atom ++ " is an element of the labels of " ++ node)] 
witness node ts (Not p)                = counterExample node ts p
witness node _  (BinaryOp And p q)     = return [(node, modelsText True node p ++ ", " ++ modelsText True node q)] 
witness node ts (BinaryOp Or p q)      = do satP <- sat ts p
                                            let form = if node `Set.member` satP then p else q
                                            return [(node, modelsText True node form)]

witness node _  (BinaryOp Implies p q) = return [(node, modelsText True node (BinaryOp Implies p q))] 

witness node _  (UQuantifier AC p)     = return [(node, "All neighboors of " ++ node ++ " satisfy " ++ ppFormula p)]          
witness node ts (UQuantifier EC p)     = do satP <- sat ts p
                                            neighs <- getNeighboors ts node
                                            goodNeigh <- choice (neighs `Set.intersection` satP)
                                            return [ (node, "Starting node"),(goodNeigh, modelsText True goodNeigh p)]

witness node _  (UQuantifier AR p)     = return [(node, "All traces starting at " ++ node ++ " reach an state that satifies " ++ ppFormula p)]          
witness node ts (UQuantifier ER p)     = findPathToSatFormula ts node p
witness node _  (UQuantifier AS p)     = return [(node, "All traces starting at " ++ node ++ " do not reach an state that does not satifies " ++ ppFormula p)]          
witness node ts (UQuantifier ES p)     = findCycleInSatFormula ts node p 
witness node _  (BQuantifier AU _ _)   = return [(node, "All traces starting at " ++ node ++ " satisfy the semantic of the until operator ")]          
witness node ts (BQuantifier EU p q)   = findExistUntilWitness ts node p q


choice :: MonadCTL m => Set.Set t -> m t
choice set = case Set.lookupMin set of
  Nothing -> failCTL "Choosing an element from an empty set!"
  Just v  -> return v