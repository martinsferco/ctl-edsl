module Reduce where

import MonadCTL

--  
reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr sformula)   = undefined
reduceExpr (ModelExpr trans labels) = undefined
reduceExpr (LabelExpr labels)       = undefined
reduceExpr (TransitionExpr trans)   = undefined
reduceExpr (VarExpr var)            = undefined