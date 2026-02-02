module Sat where

import Model.TSystemMethods
import Model.TSystem
import MonadCTL
import Common 
import Lang

import Control.Monad (filterM, join)

import qualified Data.Set as Set


models :: MonadCTL m => TSystem -> Formula -> m ()
models ts form = do satNodes <- sat ts form
                    if getInitialNodes ts `Set.isSubsetOf` satNodes
                    then printCTL "the model models the formula"
                    else printCTL "the model does not models the formula∀"

isSatis :: MonadCTL m => Formula -> m ()
isSatis form = failCTL "isSatis is not implemented"

isValid :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ()
isValid ts node form = do satNodes <- sat ts form
                          if node `Set.member` satNodes
                          then printCTL "is valid"
                          else printCTL "is not valid" 


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
    sat' _ f                    = failCTL $ "the sub-formula " ++ show f ++ " should be already trasnformed"


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
inev ts nodes = do nodes' <- (Set.union nodes <$> (preForAll ts nodes))
                   if nodes' == nodes then return nodes
                                      else inev ts nodes'

exUntil :: MonadCTL m => TSystem -> Nodes -> Nodes -> m Nodes
exUntil ts x y  = do y' <- Set.union y <$> (Set.intersection x <$> (preExist ts y))
                     if y' == y then return y  
                                else exUntil ts x y'

preExist :: MonadCTL m => TSystem -> Nodes -> m Nodes
preExist ts nodes = setFilterM existS (getNodes ts)
  where
    existS :: MonadCTL m => NodeIdent -> m Bool
    existS node = not <$> (Set.disjoint <$> (getNeighboors ts node) <*> pure nodes)

preForAll :: MonadCTL m => TSystem -> Nodes -> m Nodes
preForAll ts nodes = setFilterM forAllS (getNodes ts)
  where 
    forAllS :: MonadCTL m => NodeIdent -> m Bool
    forAllS node = Set.isSubsetOf <$> (getNeighboors ts node) <*> pure nodes


getAtoms :: Formula -> Atoms
getAtoms F                   = Set.empty 
getAtoms T                   = Set.empty
getAtoms (Atom atom)         = Set.singleton atom
getAtoms (Not p)             = getAtoms p
getAtoms (BinaryOp _ p q)    = Set.union (getAtoms p) (getAtoms q)
getAtoms (UQuantifier _ p)   = getAtoms p
getAtoms (BQuantifier _ p q) = Set.union (getAtoms p) (getAtoms q)

-- allTSystems :: Atoms -> [TSystem]
-- allTSystems atoms = tSystems atoms 1

-- tSystems :: Atoms -> Int -> [TSystem]
-- tSystems atoms n = tSystemGenerator atoms n ++ tSystems atoms (n + 1)

-- generates all the TSystems of size N with labels in Atoms
-- tSystemGenerator :: Atoms -> Int -> [TSystem]
-- tSystemGenerator atoms size = 
--   let nodes     = map (\n -> "_n" ++ show n) [1..size]
--       allLabels = map () nodes 
 -- para cada nodo construyo lo siguiente
 --  - un listado o conjunto de todas las posibles formas de etiquetarlo
 --  - un listado o conjunto de todas las transiciones posibles
 --
 -- cuando tengo eso de cada nodo, basicamente voy tomando uno de cada, y voy formando TSystem nuevos

--  allPossibleLabels :: Atoms -> [Atoms]
--  allPossibleLabels atoms = subsequences atoms

--  allPossibleTransitions :: Nodes -> [Nodes]
--  allPossibleTransitions nodes = subsequences nodes
 

setFilterM :: (Ord a, MonadCTL m) => (a -> m Bool) -> Set.Set a -> m (Set.Set a)
setFilterM p = fmap Set.fromList . filterM p . Set.toList
  


-- yo necesito 
--   - una formula
--   - nodos de sat
--   - tsystem

-- necesito devolver como un 'DTO' para que despues pueda ser PPrinteado

--  el contraejemplo muestra un camino que hace incumplir la formula
--  el testigo muestra un camino que hace valida la formula

data CounterExample = CounterExample
data Witness = Witness

data EvalInformation = EvalInformation

counterExample :: Nodes -> TSystem -> Formula -> CounterExample
counterExample satNodes ts F                       = undefined 
counterExample satNodes ts T                       = undefined 
counterExample satNodes ts (Atom atom )            = undefined  
counterExample satNodes ts (Not p)                 = undefined  
counterExample satNodes ts (BinaryOp op p q)       = undefined 
counterExample satNodes ts (UQuantifier quant p)   = undefined 
counterExample satNodes ts (BQuantifier quant p q) = undefined 


witness :: Nodes -> TSystem -> Formula -> Witness
witness satNodes ts F                       = undefined
witness satNodes ts T                       = undefined
witness satNodes ts (Atom atom )            =  undefined
witness satNodes ts (Not p)                 = undefined
witness satNodes ts (BinaryOp op p q)       = undefined
witness satNodes ts (UQuantifier quant p)   = undefined
witness satNodes ts (BQuantifier quant p q) = undefined
