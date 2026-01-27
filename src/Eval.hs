module Eval where

import MonadCTL
import AST

import qualified Data.Set as Set
import qualified Data.Map as Map

reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr sformula)   = Formula <$> replaceVarsFormula sformula
reduceExpr (ModelExpr trans labels) = 
  do  vTrans <- reduceExpr trans
      vLabels <- reduceExpr labels
      case (vTrans, vLabels) of
       (Transitions t, Labels l) -> buildTSystem t l
       _ -> failCTL "type error while reducing expr"

reduceExpr (LabelExpr labels)       = Labels <$> collectByNodes labels
reduceExpr (TransitionExpr trans)   = Transitions <$> collectByNodes trans
reduceExpr (VarExpr var)            = searchDef var



replaceVarsFormula :: MonadCTL m => SFormula -> m Formula
replaceVarsFormula SF                      = F
replaceVarsFormula ST                      = T
replaceVarsFormula (SAtom atom)            = Atom atom
replaceVarsFormula (SNot sp)               = Not <$> replaceVarsFormula sp

replaceVarsFormula (SBinaryOp bop sp sq)   = do p <- replaceVarsFormula sp
                                                q <- replaceVarsFormula sq
                                                return $ BinaryOp bop p q
replaceVarsFormula (SUQuantifier uq sp)    = UQuantifier uq <$> replaceVarsFormula sp

replaceVarsFormula (SBQuantifier bq sp sq) = do p <- replaceVarsFormula sp
                                                q <- replaceVarsFormula sq
                                                return $ BQuantifier bq p q

replaceVarsFormula (SVar var)              = 
  do  varForm <- searchDef
      case varForm of 
        (Formula f) -> return f
        _           -> failCTL "type error while reducint the expression"
        


collectByNodes :: [(NodeIdent, [b])] -> [(NodeIdent, [b])]
collect = M.toList . M.fromListWith (++)
