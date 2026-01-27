module Common where

import qualified Data.Set as Set

type VarIdent = String
type AtomIdent = String
type NodeIdent = String

type Vars  = Set.Set VarIdent
type Atoms = Set.Set AtomIdent
type Nodes = Set.Set NodeIdent

type Label = (NodeIdent, [AtomIdent])
type Neighboors = [NodeIdent]
type Transition = (NodeIdent, Bool, Neighboors)

type Definition = (VarIdent, Type, Value)
