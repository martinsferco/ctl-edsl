module Sat where

import AST
import Model.TSystem
import Model.TSystemMethods
import Common 

import MonadCTL

import qualified Data.Set as Set


models :: MonadCTL m => TSystem -> Formula -> m ()
models ts form = let satNodes = sat ts form
                 in if getInitialNodes ts `Set.isSubsetOf` satNodes
                    then printCTL "the model models the formula"
                    else printCTL "the model does not models the formula"

isSatis :: MonadCTL m => Formula -> m ()
isSatis form = failCTL "isSatis is not implemented"

isValid :: MonadCTL m => TSystem -> NodeIdent -> Formula -> m ()
isValid ts node form = let satNodes = sat ts form
                       in if node `Set.member` satNodes
                          then printCTL "is valid"
                          else printCTL "is not valid" 



sat :: TSystem -> Formula -> Nodes
sat ts = (sat' ts) . transform
  where 
    sat' _  F                   = Set.empty
    sat' _  T                   = getNodes ts
    sat' m (Atom a)             = Set.filter (isLabel ts a) (getNodes ts)
    sat' m (Not p)              = Set.difference (getNodes ts) (sat ts p)
    sat' m (BinaryOp And p q)   = Set.intersection (sat ts p) (sat ts q)
    sat' m (UQuantifier EC p)   = preExist ts (sat ts p)
    sat' m (UQuantifier AC p)   = preForAll ts (sat ts p)
    sat' m (UQuantifier AR p)   = inev ts (sat ts p)
    sat' m (BQuantifier EU p q) = exUntil ts (sat ts p) (sat ts q)
    sat' _ _                    = error "It should be transformed already"


transform :: Formula -> Formula
transform F                      = F
transform T                      = T
transform a@(Atom _)             = a
transform (Not p)                = Not (transform p)
transform (BinaryOp And p q)     = BinaryOp And (transform p) (transform q)

transform (BinaryOp Or p q)      = let tp = Not (transform p)
                                       tq = Not (transform q)
                                   in Not (BinaryOp And tp tq)
  
transform (BinaryOp Implies p q) = let tp = transform p
                                       tq = Not (transform q)
                                   in Not (BinaryOp And tp tq)
  
transform (UQuantifier AC p)     = UQuantifier AC $ transform p
transform (UQuantifier EC p)     = UQuantifier EC $ transform p
transform (UQuantifier AR p)     = UQuantifier AR $ transform p
transform (UQuantifier ER p)     = BQuantifier EU T $ transform p
transform (UQuantifier AS p)     = Not (BQuantifier EU T (Not $ transform p))
transform (UQuantifier ES p)     = Not (UQuantifier AR (Not $ transform p))

transform (BQuantifier AU p q)   = let tp = transform p
                                       tq = transform q
                                       neitherPQ = BinaryOp And (Not tp) (Not tq)
                                       untilPath = Not (BQuantifier EU (Not tq) neitherPQ) 
                                       inevitableQ = UQuantifier AR $ transform q
                                   in BinaryOp And untilPath inevitableQ

transform (BQuantifier EU p q)   = BQuantifier EU (transform p) (transform q)


inev :: TSystem -> Nodes -> Nodes
inev ts nodes = let nodes' = Set.union nodes (preForAll ts nodes)
                in if nodes' == nodes then nodes
                                      else inev ts nodes'

exUntil :: TSystem -> Nodes -> Nodes -> Nodes
exUntil ts x y  = let y' = Set.union y (Set.intersection x (preExist ts y))
                  in if y' == y then y else exUntil ts x y'


forAllUntil :: TSystem -> Nodes -> Nodes -> Nodes
forAllUntil = undefined

preExist :: TSystem -> Nodes -> Nodes
preExist ts nodes = Set.filter existS (getNodes ts)
  where
    existS :: NodeIdent -> Bool
    existS node = not $ Set.disjoint (getNeighboors ts node) nodes

preForAll :: TSystem -> Nodes -> Nodes
preForAll ts nodes = Set.filter forAllS $ (getNodes ts)
  where 
    forAllS :: NodeIdent -> Bool
    forAllS node = Set.isSubsetOf (getNeighboors ts node) nodes