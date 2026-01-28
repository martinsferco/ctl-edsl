module AST where

import Common
import Model.TSystem


data Type
  = ModelTy
  | LabelsTy
  | NodesTy
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


data Expr 
  = FormulaExpr SFormula   
  | ModelExpr Expr Expr
  | LabelsExpr [Label]
  | NodesExpr [InfoNode]
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
  | IsValid Expr                      --     |= p
  | Models Expr Expr                  -- M   |= p
  -- | ModelsNode Expr NodeIdent Expr -- M,s |= p
  deriving (Show, Eq)


type Program = [Sentence]


data Value
  = Formula Formula
  | Model TSystem
  | Labels LabelingFunction
  | Nodes InfoNodes
  deriving (Show, Eq)