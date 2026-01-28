module Eval where

import MonadCTL
import AST
import Common

import qualified Data.Set as Set
import qualified Data.Map as Map

reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr sformula)   = Formula <$> replaceVarsFormula sformula
reduceExpr (ModelExpr trans labels) = 
  do  vTrans <- reduceExpr trans
      vLabels <- reduceExpr labels
      case (vTrans, vLabels) of
      --  (Transitions t, Labels l) -> buildTSystem t l
       (Nodes t, Labels l) -> failCTL "ty"
       _ -> failCTL "type error while reducing expr"

reduceExpr (LabelsExpr labels)      = return $ Labels (collectByNodes labels)
reduceExpr (NodesExpr nodes)        = return $ Nodes (constructInfonodes nodes)
reduceExpr (VarExpr var)            = searchDef var



replaceVarsFormula :: MonadCTL m => SFormula -> m Formula
replaceVarsFormula SF                      = return F
replaceVarsFormula ST                      = return T
replaceVarsFormula (SAtom atom)            = return $ Atom atom
replaceVarsFormula (SNot sp)               = Not <$> replaceVarsFormula sp

replaceVarsFormula (SBinaryOp bop sp sq)   = do p <- replaceVarsFormula sp
                                                q <- replaceVarsFormula sq
                                                return $ BinaryOp bop p q
replaceVarsFormula (SUQuantifier uq sp)    = UQuantifier uq <$> replaceVarsFormula sp

replaceVarsFormula (SBQuantifier bq sp sq) = do p <- replaceVarsFormula sp
                                                q <- replaceVarsFormula sq
                                                return $ BQuantifier bq p q

replaceVarsFormula (SVar var)              = 
  do  varForm <- searchDef var
      case varForm of 
        (Formula f) -> return f
        _           -> failCTL "type error while reducint the expression"
        

constructInfonodes :: [InfoNode] -> InfoNodes
constructInfonodes info = (Set.fromList initialNodes, transFunction)
  where 
    initialNodes = map (\(n, _, _) -> n) (filter (\(_, initial, _) -> initial) info) 
    transFunction = collectByNodes (map (\(n, _, neigh) -> (n,neigh)) info)

collectByNodes :: Ord b => [(NodeIdent, [b])] -> Map.Map NodeIdent (Set.Set b)
collectByNodes = Map.fromListWith Set.union . map (\(n, bs) -> (n, Set.fromList bs))