module AST where

import TSystem
import Common


data Type
   = ModelTy
   | LabelsTy
   | TransitionsTy
   | FormulaTy
   deriving (Show, Eq)


data UQuantifier -- Temporal unary quantifiers
  = AC -- Forall next
  | EC -- Exists next
  | AR -- Inevitable
  | ER -- Possible
  | AS -- Invariant
  | ES -- Invariant for some trace
  deriving (Show, Eq)

data BQuantifier -- Temporal binary quantifiers
  = AU -- Forall until
  | EU -- Exists until
  deriving (Show, Eq)

data BinaryOp
  = And
  | Or
  | Implies
  deriving (Show, Eq)

type Label = (NodeIdent, [AtomIdent])
type Neighboors = [NodeIdent]
type Transition = (NodeIdent, Bool, Neighboors)

data Expr 
  = FormulaExpr SFormula   
  | ModelExpr Expr Expr
  | LabelExpr [Label]
  | TransitionExpr [Transition]
  | VarExpr VarIdent
  deriving (Show, Eq)

data SFormula
  = SF
  | ST
  | SAtom AtomIdent
  | SNot SFormula
  | SBinaryOp BinaryOp SFormula SFormula
  | SUQuantifier UQuantifier SFormula
  | SBQuantifier BQuantifier SFormula SFormula
  | SVar VarIdent
  deriving (Show, Eq)

data Formula
  = F
  | T
  | Atom AtomIdent
  | Not Formula
  | BinaryOp BinaryOp Formula Formula
  | UQuantifier UQuantifier Formula
  | BQuantifier BQuantifier Formula Formula
  deriving (Show, Eq)

data Sentence 
  = Def VarIdent Type Expr
  | Export Expr
  | IsValid Expr
  | Models Expr Expr
  -- | ModelsNode Expr NodeIdent Expr
  deriving (Show, Eq)

data Value
  = Formula Formula
  | Model TSystem
  | Labels [Label]
  | Transitions [Transition]
  deriving (Show, Eq)

type Program = [Sentence]