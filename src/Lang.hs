module Lang 
  ( Type (..)
  , UQuantifier (..)
  , BQuantifier (..)
  , BinaryOp (..)
  , Expr  (..)
  , SFormula (..)
  , Formula (..)
  , Sentence  (..)
  , Program
  , Value (..) ) where

import Model.TSystem ( TSystem )
import Common        ( Pos, Label, InfoNode, VarIdent, NodeIdent, AtomIdent, LabelingFunction, InfoNodes )


data Type -- Types of the expressions and values
  = ModelTy
  | LabelsTy
  | NodesTy
  | FormulaTy
  deriving (Eq, Show)

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
  = FormulaExpr Pos SFormula   
  | ModelExpr Pos Expr Expr
  | LabelsExpr Pos [Label]
  | NodesExpr Pos [InfoNode]
  | VarExpr Pos VarIdent
  deriving Show

data SFormula -- Superfitial formulas
  = SF 
  | ST 
  | SAtom AtomIdent
  | SNot SFormula
  | SBinaryOp BinaryOp SFormula SFormula
  | SUQuantifier UQuantifier SFormula
  | SBQuantifier BQuantifier SFormula SFormula
  | SVar VarIdent
  deriving Show


data Formula -- Formulas
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
  | IsSatis Pos Expr String 
  | Models Pos Expr Expr                  
  | IsValid Pos Expr NodeIdent Expr
  deriving Show


type Program = [Sentence]


data Value
  = Formula { formula :: Formula }
  | Model   { model   :: TSystem }
  | Labels  { labels  :: LabelingFunction }
  | Nodes   { nodes   :: InfoNodes }
  deriving Show