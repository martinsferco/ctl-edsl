module Common where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Identifiers
type VarIdent = String
type AtomIdent = String
type NodeIdent = String

-- Sets of identifiers
type Vars  = Set.Set VarIdent
type Atoms = Set.Set AtomIdent
type Nodes = Set.Set NodeIdent

-- 
type Label = (NodeIdent, [AtomIdent])
type Neighboors = [NodeIdent]
type InfoNode = (NodeIdent, Bool, Neighboors)

-- Definition store in the monad's state

-- Useful for TSystem definition
type TransitionFunction = Map.Map NodeIdent Nodes
type LabelingFunction   = Map.Map NodeIdent Atoms

-- Initial nodes and transition function
type InfoNodes = (Nodes, TransitionFunction)
