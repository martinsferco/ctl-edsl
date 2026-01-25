module AST where

type VarIdent = String
type AtomIdent = String
type NodeIdent = String

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
  = FormulaExpr Formula   
  | ModelExpr Expr Expr
  | LabelExp [Label]
  | TransitionExp [Transition]
  | VarExp VarIdent
  deriving (Show, Eq)

data Formula
  = F
  | T
  | Atom AtomIdent
  | Not Formula
  | BinaryOp BinaryOp Formula Formula
  | UQuantifier UQuantifier Formula
  | BQuantifier BQuantifier Formula Formula
  | Var VarIdent
  deriving (Show, Eq)

data Sentence 
  = Def VarIdent Type Expr
  | Export Expr
  | IsValid Expr
  | Models Expr Expr
  deriving (Show, Eq)

type Program = [Sentence]