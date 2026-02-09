module Common where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Position of a sentence inside a file
data Pos = NoPos | Pos !Int 

instance Show Pos where
  show NoPos      = ""
  show (Pos line) = "(Line " ++ show line ++ ")" 

-- Identifiers of Variables, Atoms and Nodes
type VarIdent = String
type AtomIdent = String
type NodeIdent = String

-- Sets of identifiers
type Vars  = Set.Set VarIdent
type Atoms = Set.Set AtomIdent
type Nodes = Set.Set NodeIdent

-- Useful for the parsers' definitions
type Label = (NodeIdent, [AtomIdent])
type Neighboors = [NodeIdent]
type InfoNode = (NodeIdent, Bool, Neighboors)

-- Useful for TSystem and Graph definitions
type TransitionFunction = Map.Map NodeIdent Nodes
type LabelingFunction   = Map.Map NodeIdent Atoms

-- Initial nodes and transition function
data InfoNodes = InfoNodes
  {
    initialNodes :: Nodes,
    transitions :: TransitionFunction
  }
  deriving (Show, Eq)



