module Lang where

import Model.TSystem
import Common


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
  deriving Show

data BQuantifier -- Temporal binary quantifiers
  = AU -- Forall until
  | EU -- Exists until
  deriving Show

data BinaryOp
  = And
  | Or
  | Implies
  deriving Show


data Expr 
  = FormulaExpr SFormula   
  | ModelExpr Expr Expr
  | LabelsExpr [Label]
  | NodesExpr [InfoNode]
  | VarExpr VarIdent
  deriving Show


data SFormula
  = SF 
  | ST 
  | SAtom AtomIdent
  | SNot SFormula
  | SBinaryOp BinaryOp SFormula SFormula
  | SUQuantifier UQuantifier SFormula
  | SBQuantifier BQuantifier SFormula SFormula
  | SVar VarIdent
  deriving Show


data Formula
  = F
  | T
  | Atom AtomIdent
  | Not Formula
  | BinaryOp BinaryOp Formula Formula
  | UQuantifier UQuantifier Formula
  | BQuantifier BQuantifier Formula Formula
  deriving Show


data Sentence 
  = Def Pos VarIdent Type Expr
  | Export Pos Expr String
  | IsSatis Pos Expr                      --     |= p
  | Models Pos Expr Expr                  -- M   |= p
  | IsValid Pos Expr NodeIdent Expr
  deriving Show


type Program = [Sentence]


data Value
  = Formula { formula :: Formula }
  | Model   { model   :: TSystem }
  | Labels  { labels  :: LabelingFunction }
  | Nodes   { nodes   :: InfoNodes }
  deriving Show
