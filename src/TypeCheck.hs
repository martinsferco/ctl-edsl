module TypeCheck where

import MonadCTL
import Common
import Lang

import Control.Monad (unless)

import qualified Data.Set as Set


exprOfType :: MonadCTL m => Expr -> Type -> m Bool
exprOfType expr ty = do exprTy <- findTypeExpr expr
                        return (exprTy == ty)

valueOfType :: Value -> Type -> Bool
valueOfType value ty = findTypeValue value == ty

findTypeExpr :: MonadCTL m => Expr -> m Type
findTypeExpr (ModelExpr p t l)   = do unlessCTL (exprOfType t NodesTy) $
                                        failPosCTL p "Expected a Nodes' expression."
                                      unlessCTL (exprOfType l LabelsTy) $
                                        failPosCTL p "Expected a Labels' expression."
                                      return ModelTy
                                      
findTypeExpr (LabelsExpr _ _)    = return LabelsTy
findTypeExpr (NodesExpr _ _)     = return NodesTy

findTypeExpr (VarExpr _ var)      = getTy var

findTypeExpr (FormulaExpr _ f)   = do let fv = Set.toList $ freeVariables f
                                      mapM_ checkFormulaVarTy fv
                                      return FormulaTy

    where checkFormulaVarTy :: MonadCTL m => VarIdent -> m ()
          checkFormulaVarTy var = do varTy <- getTy var
                                     unless (varTy == FormulaTy) 
                                            (failCTL $ "The variable " ++ show var ++ " should refer to a Formula.")
 

findTypeValue :: Value -> Type
findTypeValue (Model _)   = ModelTy
findTypeValue (Labels _)  = LabelsTy
findTypeValue (Formula _) = FormulaTy
findTypeValue (Nodes _)   = NodesTy

freeVariables :: SFormula -> Set.Set VarIdent
freeVariables SF                   = Set.empty
freeVariables ST                   = Set.empty
freeVariables (SAtom _)            = Set.empty
freeVariables (SNot p)             = freeVariables p

freeVariables (SBinaryOp _ p q)    = let varP = freeVariables p
                                         varQ = freeVariables q
                                     in Set.union varP varQ

freeVariables (SUQuantifier _ p)   = freeVariables p

freeVariables (SBQuantifier _ p q) = let varP = freeVariables p
                                         varQ = freeVariables q
                                     in Set.union varP varQ

freeVariables (SVar var)           = Set.singleton var

typeCheckSentence :: MonadCTL m => Sentence -> m() 
typeCheckSentence (Def p _ ty expr)        = unlessCTL (expr `exprOfType` ty) $
                                                failPosCTL p $ "The expression of the definition is not of type " ++ show ty

typeCheckSentence (Export p model _)       = unlessCTL (model `exprOfType` ModelTy) $
                                                failPosCTL p "Expected a model expression, but got a different type. You can only export models."

typeCheckSentence (IsSatis p formula)      = unlessCTL (formula `exprOfType` FormulaTy) $
                                                failPosCTL p "Expected a CTL formula, but got a different type."

typeCheckSentence (Models p model formula) = do unlessCTL (model `exprOfType` ModelTy) $
                                                    failPosCTL p $ "Expected a model expression, but got a different type."
                                                unlessCTL (formula `exprOfType` FormulaTy) $
                                                    failPosCTL p "Expected a CTL formula, but got a different type."

typeCheckSentence (IsValid p model _ form) = do unlessCTL (model `exprOfType` ModelTy) $
                                                    failPosCTL p "Expected a model expression, but got a different type."
                                                unlessCTL (form `exprOfType` FormulaTy) $
                                                    failPosCTL p "Expected a CTL formula, but got a different type."

