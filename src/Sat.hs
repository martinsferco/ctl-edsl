module Sat
  ( models
  , isValid
  , isSatis) where

import Model.TSystemMethods ( getInitialNodes, setInitialNodes, getNodes, isLabel, getNeighboors, 
                              getNeighboors, findCycleInSubgraph, findPathToTargetInSubgraph )
import PrettyPrinter        ( ppFormula, )
import Model.TSystem        ( TSystem (..), Graph (..) )
import EvalResult           ( EvalResult (..), ExamplePath, CheckNodeType (..) )
import MonadCTL             ( MonadCTL, failCTL )
import Common               ( NodeIdent, Atoms, TransitionFunction, Nodes, LabelingFunction )
import Lang                 ( Formula (..), BQuantifier (..), UQuantifier (..), BinaryOp (..) )

import Control.Monad ( filterM, join )

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (subsequences)


--------------------------------------------------------------------------------
-- Main functionalities of the SAT module.
--------------------------------------------------------------------------------

-- Checks if a certain Transition System models a formula.
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

-- Checks if certain formula is valid in a Transition System, starting in a given node.
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

-- Checks if a certain formula is valid for some Transition System.
-- !! WARNING: This function it is not efficient at all, because it test the formula
-- !!          against all possible models. It could algo go on forever if the formula
-- !!          is not satisfiable.
-- !!          In a future we can implement a limit, taking into account the concept
-- !!          of the 'length' of a formula.
isSatis :: MonadCTL m => Formula -> m (Maybe TSystem)
isSatis form = isSatis' (tSystems $ getAtoms form) form

  where isSatis' :: MonadCTL m => [TSystem] -> Formula -> m (Maybe TSystem)
        isSatis' []       _ = return Nothing
        isSatis' (ts:tss) f = do satF <- sat ts f
                                 if satF /= Set.empty then return $ Just (setInitialNodes ts satF)
                                                          else isSatis' tss f

-- Calculates the nodes of a Transition System where the formula is satisfiable.
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

-- Transforms the formula in another with the same semantic. 
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

-- Inev procedure
inev :: MonadCTL m => TSystem -> Nodes -> m Nodes
inev ts x = do x' <- (Set.union x <$> (preForAll ts x))
               if x' == x then return x
                          else inev ts x'
-- Ex-until procedure.
exUntil :: MonadCTL m => TSystem -> Nodes -> Nodes -> m Nodes
exUntil ts x y  = do y' <- Set.union y <$> (Set.intersection x <$> (preExist ts y))
                     if y' == y then return y  
                                else exUntil ts x y'

-- Gives all the nodes of the Transition System that have a neighboor in x.
preExist :: MonadCTL m => TSystem -> Nodes -> m Nodes
preExist ts x = setFilterM existS (getNodes ts)
  where
    existS :: MonadCTL m => NodeIdent -> m Bool
    existS node = not <$> (Set.disjoint <$> (getNeighboors ts node) <*> pure x)

-- Gives all the nodes of the Transition System that have all of its neighboors in x.
preForAll :: MonadCTL m => TSystem -> Nodes -> m Nodes
preForAll ts x = setFilterM forAllS (getNodes ts)
  where 
    forAllS :: MonadCTL m => NodeIdent -> m Bool
    forAllS node = Set.isSubsetOf <$> (getNeighboors ts node) <*> pure x

-- Gets all the atoms of a given formula.
getAtoms :: Formula -> Atoms
getAtoms F                   = Set.empty 
getAtoms T                   = Set.empty
getAtoms (Atom atom)         = Set.singleton atom
getAtoms (Not p)             = getAtoms p
getAtoms (BinaryOp _ p q)    = Set.union (getAtoms p) (getAtoms q)
getAtoms (UQuantifier _ p)   = getAtoms p
getAtoms (BQuantifier _ p q) = Set.union (getAtoms p) (getAtoms q)

--------------------------------------------------------------------------------
-- Construction of all possible Transition Systems
--------------------------------------------------------------------------------
tSystems :: Atoms -> [TSystem]
tSystems atoms = take 1000000 (concat $ map (\k -> tSystemGenerator atoms k) [1..])

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

-- Generates all the Transition System for a set of atoms an a given number of nodes.
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
  

--------------------------------------------------------------------------------
-- Counter-Example and Witnesses
--------------------------------------------------------------------------------

-- Builds a counter-example path for the given formula, in a Transition System, starting at a particular node.
-- * Remark: the function asummes that the formula is not valid for the given node and TSystem.
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
                                                   return [ (node, "Starting node"), (badNeigh, modelsText False badNeigh p)]

counterExample node _  (UQuantifier EC p)     = return [(node, "All neighboors of " ++ node ++ " do not satisfy " ++ ppFormula p)]         
counterExample node ts (UQuantifier AR p)     = findCycleFormula OutsideSat ts node p 
counterExample node _  (UQuantifier ER p)     = return [(node, "All traces starting at " ++ node ++ ", never reach an state that satifies " ++ ppFormula p)]         
counterExample node ts (UQuantifier AS p)     = findPathFormula OutsideSat ts node p
counterExample node _  (UQuantifier ES p)     = return [(node, "All traces starting at " ++ node ++ ", reach an state that does not satisfy " ++ ppFormula p)]         
counterExample node ts (BQuantifier AU p q)   = findForallUntilCounterExample ts node p q
counterExample node _  (BQuantifier EU _ _)   = return [(node, "All traces starting at " ++ node ++ ", do not satify the semantic of the Until operator")]         


-- Builds a witness path for the given formula, in a Transition System, starting at a particular node.
-- * Remark: the function asummes that the formula is valid for the given node and TSystem.
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
                                            return [ (node, "Starting node"), (goodNeigh, modelsText True goodNeigh p)]

witness node _  (UQuantifier AR p)     = return [(node, "All traces starting at " ++ node ++ " reach an state that satifies " ++ ppFormula p)]          
witness node ts (UQuantifier ER p)     = findPathFormula InsideSat ts node p
witness node _  (UQuantifier AS p)     = return [(node, "All traces starting at " ++ node ++ " never reach an state that does not satisfy " ++ ppFormula p)]          
witness node ts (UQuantifier ES p)     = findCycleFormula InsideSat ts node p 
witness node _  (BQuantifier AU _ _)   = return [(node, "All traces starting at " ++ node ++ " satisfy the semantic of the Until operator ")]          
witness node ts (BQuantifier EU p q)   = findExistUntilWitness ts node p q


--------------------------------------------------------------------------------
-- Cycles and paths for counter-examples and witnesess
--------------------------------------------------------------------------------

data SearchMode = InsideSat | OutsideSat

findCycleFormula :: MonadCTL m => SearchMode -> TSystem -> NodeIdent -> Formula -> m ExamplePath
findCycleFormula searchMode ts start form = do
  let (form', msg) = case searchMode of 
                      InsideSat  -> (form, "satisfiable.")
                      OutsideSat -> (Not form, "not satisfiable.")

  satForm' <- sat ts form'
  maybeCicle <- findCycleInSubgraph ts satForm' start

  case maybeCicle of
    Just cycleExample -> return $ addCycleDescription form' cycleExample
    Nothing           -> failCTL $ "No cycle found in the set of nodes in which the the formula is " ++ msg
  
findPathFormula :: MonadCTL m => SearchMode -> TSystem -> NodeIdent -> Formula -> m ExamplePath
findPathFormula searchMode ts start form = do

  let (form', formPath, msg) = case searchMode of
                                InsideSat  -> (form, Not form, "satisfy")
                                OutsideSat -> (Not form, form, "not satisfy")

  satForm' <- sat ts form'
  maybePath <- findPathToTargetInSubgraph ts (getNodes ts) start satForm'
  case maybePath of 
    Just path -> return $ addPathDescription formPath form' path
    Nothing -> failCTL $ "No path found that reaches a set of nodes that do " ++ msg ++ " the formula."


findForallUntilCounterExample :: MonadCTL m => TSystem -> NodeIdent -> Formula -> Formula -> m ExamplePath
findForallUntilCounterExample ts start p q = do 
  let pNotQ = BinaryOp And p (Not q)
  let notPNotQ = BinaryOp And (Not p) (Not q)
  satPNotQ <- sat ts pNotQ 
  satNotPNotQ <- sat ts notPNotQ
  maybeCycle  <- findCycleInSubgraph ts satPNotQ start -- Searchs for a Cycle of (p && !q)
  case maybeCycle of
   Just cycleExample -> return $ addCycleDescription pNotQ cycleExample
   Nothing           -> do
     maybePath <- findPathToTargetInSubgraph ts satPNotQ start satNotPNotQ -- Searchs for a path of (p && !q) until (p! && !q)
     case maybePath of
       Just path -> return $ addPathDescription pNotQ notPNotQ path
       Nothing -> failCTL "No counterexample found for the forall until operator."

findExistUntilWitness :: MonadCTL m => TSystem -> NodeIdent -> Formula -> Formula -> m ExamplePath
findExistUntilWitness ts start p q = do 
  satP <- sat ts p 
  satQ <- sat ts q
  maybePath <- findPathToTargetInSubgraph ts satP start satQ -- Searchs for a path of p until q
  case maybePath of
    Just path -> return $ addPathDescription p q path
    Nothing -> failCTL "No witness found for the exist until operator."

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

addCycleDescription :: Formula -> [NodeIdent] -> ExamplePath
addCycleDescription form c = map (\n -> (n, modelsText True n form)) (init c) ++ 
                                    [(last c, modelsText True (last c) form ++ " - Cycle detection")]

addPathDescription :: Formula -> Formula -> [NodeIdent] -> ExamplePath
addPathDescription form endFormula path = map (\n -> (n, modelsText True n form)) (init path) ++ 
                                             [(last path, modelsText True (last path) endFormula)]

modelsText :: Bool -> NodeIdent -> Formula -> String
modelsText isModel node form = "M," ++ node ++ " " ++ symbol ++ ppFormula form
  where symbol = if isModel then "⊨ " else "⊭ "


choice :: MonadCTL m => Set.Set t -> m t
choice set = case Set.lookupMin set of
  Nothing -> failCTL "Choosing an element from an empty set!"
  Just v  -> return v