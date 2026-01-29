module Eval where

import Model.TSystemMethods
import Model.TSystem
import TypeCheck
import MonadCTL
import Common
import Lang
import Sat

import qualified Data.Set as Set
import qualified Data.Map as Map



evalSentence :: MonadCTL m => Sentence -> m()
evalSentence (Def _ var ty e) = do  v <- reduceExpr e
                                    v `valueOfType` ty
                                    addDef var ty v

evalSentence (Export _ e f)   = do  v <- reduceExpr e
                                    ts <- expectsModel v
                                    exportTSystem ts f

evalSentence (IsSatis _ e)    = do  v <- reduceExpr e
                                    formula <- expectsFormula v
                                    isSatis formula

evalSentence (Models _ m f)   = do  vm <- reduceExpr m
                                    ts <- expectsModel vm
                                    vf <- reduceExpr f
                                    form <- expectsFormula vf
                                    ts `models` form

evalSentence (IsValid _ m n f) = do vm <- reduceExpr m
                                    ts <- expectsModel vm
                                    vf <- reduceExpr f
                                    form <- expectsFormula vf
                                    isValid ts n form

reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr sformula)   = Formula <$> replaceVarsFormula sformula
reduceExpr (ModelExpr nodes labels) = do  vNodes <- reduceExpr nodes
                                          infoNodes <- expectsNodes vNodes
                                          vLabels <- reduceExpr labels
                                          labels <- expectsLabels vLabels
                                          Model <$> buildTSystem infoNodes labels

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

replaceVarsFormula (SVar var)              = do varForm <- searchDef var
                                                expectsFormula varForm



constructInfonodes :: [InfoNode] -> InfoNodes
constructInfonodes info = InfoNodes (Set.fromList initialNodes) transFunction
  where initialNodes = [ n | (n, initial, _) <- info, initial ]
        transFunction = collectByNodes [ (n, neigh) | (n, _, neigh) <- info ]

collectByNodes :: Ord b => [(NodeIdent, [b])] -> Map.Map NodeIdent (Set.Set b)
collectByNodes = Map.fromListWith Set.union . map (\(n, bs) -> (n, Set.fromList bs))




-- Our language has a TypeChecker, so it should not be any problem
-- with this. We do it in case it fails.
expectsModel :: MonadCTL m => Value -> m TSystem
expectsModel v = v `valueOfType` ModelTy >> return (model v)

expectsLabels :: MonadCTL m => Value -> m LabelingFunction
expectsLabels v = v `valueOfType` LabelsTy >> return (labels v)

expectsNodes :: MonadCTL m => Value -> m InfoNodes
expectsNodes v = v `valueOfType` NodesTy >> return (Lang.nodes v)

expectsFormula :: MonadCTL m => Value -> m Formula
expectsFormula v = v `valueOfType` FormulaTy >> return (formula v)