module Eval where

import Model.TSystemMethods
import Model.TSystem
import PrettyPrinter
import TypeCheck
import MonadCTL
import Common
import Error
import Lang
import Sat

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad ( unless )


evalSentence :: MonadCTL m => Sentence -> m()
evalSentence d@(Def _ var ty e) = addDefinition d

evalSentence (Export _ e f)     = do  v <- reduceExpr e
                                      ts <- expectsModel v
                                      exportTSystem ts f

evalSentence (IsSatis _ e)      = do  v <- reduceExpr e
                                      formula <- expectsFormula v
                                      maybeTS <- isSatis formula
                                      maybeExportTSystem maybeTS "CHADDY"
                                      
evalSentence (Models _ m f)   =   do  vm <- reduceExpr m
                                      ts <- expectsModel vm
                                      vf <- reduceExpr f
                                      form <- expectsFormula vf
                                      evalResult <- ts `models` form
                                      printCTL $ ppEvalResult evalResult

evalSentence (IsValid _ m n f) =  do vm <- reduceExpr m
                                     ts <- expectsModel vm
                                     vf <- reduceExpr f
                                     form <- expectsFormula vf
                                     evalResult <- isValid ts n form
                                     printCTL $ ppEvalResult evalResult



maybeExportTSystem :: MonadCTL m => Maybe TSystem -> String -> m ()
maybeExportTSystem mts name = 
  case mts of
    Nothing -> printCTL "[] Did not found a transition system which models the formula."
    Just ts -> exportTSystem ts name  


addDefinition :: MonadCTL m => Sentence -> m ()
addDefinition (Def _ var ty e) = do v <- reduceExpr e
                                    unless (v `valueOfType` ty) $
                                      failCTL $ ("Runtime error: " ++ notOfTypeMsg var ty)
                                    addDef var ty v
addDefinition _ = return ()


reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr _ sformula)   = Formula <$> replaceVarsFormula sformula
reduceExpr (ModelExpr _ nodes labels) = do  vNodes <- reduceExpr nodes
                                            infoNodes <- expectsNodes vNodes
                                            vLabels <- reduceExpr labels
                                            labels <- expectsLabels vLabels
                                            Model <$> buildTSystem infoNodes labels

reduceExpr (LabelsExpr _ labels)      = return $ Labels (collectByNodes labels)
reduceExpr (NodesExpr _ nodes)        = return $ Nodes (constructInfonodes nodes)
reduceExpr (VarExpr _ var)            = searchDef var



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
expectsModel v = do unless (v `valueOfType` ModelTy) $
                      failCTL ("Runtime error: " ++ expectedMsg ModelTy)
                    return (model v)

expectsLabels :: MonadCTL m => Value -> m LabelingFunction
expectsLabels v = do  unless (v `valueOfType` LabelsTy) $
                        failCTL ("Runtime error: " ++ expectedMsg LabelsTy)
                      return (labels v)

expectsNodes :: MonadCTL m => Value -> m InfoNodes
expectsNodes v = do unless (v `valueOfType` NodesTy) $
                      failCTL ("Runtime error: " ++ expectedMsg NodesTy)
                    return (Lang.nodes v)

expectsFormula :: MonadCTL m => Value -> m Formula
expectsFormula v = do unless (v `valueOfType` FormulaTy) $
                        failCTL ("Runtime error: " ++ expectedMsg FormulaTy)
                      return (formula v)