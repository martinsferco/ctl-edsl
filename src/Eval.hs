module Eval
  ( evalSentence
  , addDefinition ) where

import Model.TSystemMethods ( exportTSystem, buildTSystem )
import Model.TSystem        ( TSystem )
import PrettyPrinter        ( ppEvalResult, )
import TypeCheck            ( valueOfType )
import MonadCTL             ( MonadCTL, printCTL, failCTL, addDef, searchDef  )
import Common               ( InfoNode, InfoNodes (..), NodeIdent, LabelingFunction )
import Error                ( notOfTypeMsg, expectedMsg )
import Lang                 ( Sentence (..), Expr (..), Value (..), SFormula (..), Formula (..), Type (..) ) 
import Sat                  ( isSatis, models, isValid )

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad ( unless )

-- Evals a sentence of our language.
evalSentence :: MonadCTL m => Sentence -> m()
evalSentence d@(Def _ _ _ _)  = addDefinition d

evalSentence (Export _ e f)   = do  v <- reduceExpr e
                                    ts <- expectsModel v
                                    exportTSystem ts f

evalSentence (IsSatis _ e f)  = do  v <- reduceExpr e
                                    form <- expectsFormula v
                                    maybeTS <- isSatis form
                                    maybeExportTSystem maybeTS f
                                      
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


-- Takes a sentence of the language and evaluates it only when the sentence is a definition.
addDefinition :: MonadCTL m => Sentence -> m ()
addDefinition (Def _ var ty e) = do v <- reduceExpr e
                                    unless (v `valueOfType` ty) $
                                      failCTL $ ("Runtime error: " ++ notOfTypeMsg var ty)
                                    addDef var ty v
addDefinition _ = return ()


-- Reduce and Expression of the language to a Value.
reduceExpr :: MonadCTL m => Expr -> m Value
reduceExpr (FormulaExpr _ form)   = Formula <$> replaceVarsFormula form
reduceExpr (ModelExpr _ n l)      = do vNodes <- reduceExpr n
                                       mInfoNodes <- expectsNodes vNodes
                                       vLabels <- reduceExpr l
                                       mLabels <- expectsLabels vLabels
                                       Model <$> buildTSystem mInfoNodes mLabels

reduceExpr (LabelsExpr _ l)       = return $ Labels (collectByNodes l)
reduceExpr (NodesExpr _ n)        = return $ Nodes (constructInfonodes n)
reduceExpr (VarExpr _ var)        = searchDef var


-- Replace all variables inside a superficial formula with their corresponding
-- formulas stored in the global state.
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
constructInfonodes info = InfoNodes (Set.fromList initNodes) transFunction
  where initNodes = [ n | (n, initial, _) <- info, initial ]
        transFunction = collectByNodes [ (n, neigh) | (n, _, neigh) <- info ]

collectByNodes :: Ord b => [(NodeIdent, [b])] -> Map.Map NodeIdent (Set.Set b)
collectByNodes = Map.fromListWith Set.union . map (\(n, bs) -> (n, Set.fromList bs))

-- Since the language has a type checker, this should not be an issue.
-- We include this check as a precaution.
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