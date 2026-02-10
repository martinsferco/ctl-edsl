module TypeCheck
   ( typeCheckSentence
   , valueOfType ) where

import MonadCTL ( MonadCTL, unlessCTL, failPosCTL, getTy, failCTL )
import Common   ( VarIdent )
import Error    ( expectedMsg, incorrectVarMsg, notOfTypeMsg )
import Lang     ( Sentence (..), SFormula (..), Value (..), Type (..), Expr (..) )

import Control.Monad (unless)

import qualified Data.Set as Set

-- Checks that the expression is of a certain type
exprOfType :: MonadCTL m => Expr -> Type -> m Bool
exprOfType expr ty = do exprTy <- findTypeExpr expr
                        return (exprTy == ty)

-- Checks that the value is of a certain type
valueOfType :: Value -> Type -> Bool
valueOfType value ty = findTypeValue value == ty

-- Finds the type of an expression
findTypeExpr :: MonadCTL m => Expr -> m Type
findTypeExpr (ModelExpr p t l)   = do unlessCTL (exprOfType t NodesTy) $
                                        failPosCTL p (expectedMsg NodesTy)
                                      unlessCTL (exprOfType l LabelsTy) $
                                        failPosCTL p (expectedMsg LabelsTy)
                                      return ModelTy
                                      
findTypeExpr (LabelsExpr _ _)    = return LabelsTy
findTypeExpr (NodesExpr _ _)     = return NodesTy

findTypeExpr (VarExpr _ var)      = getTy var

findTypeExpr (FormulaExpr _ f)   = do let fv = Set.toList $ freeVariables f
                                      mapM_ checkFormulaVarTy fv
                                      return FormulaTy

    where   checkFormulaVarTy :: MonadCTL m => VarIdent -> m ()
            checkFormulaVarTy var = do varTy <- getTy var
                                       unless (varTy == FormulaTy) $ 
                                               failCTL (incorrectVarMsg var FormulaTy)
 
-- Gets the Type of a Value
findTypeValue :: Value -> Type
findTypeValue (Model   _) = ModelTy
findTypeValue (Labels  _) = LabelsTy
findTypeValue (Formula _) = FormulaTy
findTypeValue (Nodes   _) = NodesTy

-- Gets the free variables of a Supertifial formulas
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

-- Type-checks a sentence
typeCheckSentence :: MonadCTL m => Sentence -> m() 
typeCheckSentence (Def p v ty expr)     = unlessCTL (expr `exprOfType` ty) $
                                            failPosCTL p $ (notOfTypeMsg v ty)

typeCheckSentence (Export p m _)        = unlessCTL (m `exprOfType` ModelTy) $
                                            failPosCTL p (expectedMsg ModelTy)

typeCheckSentence (IsSatis p form _)    = unlessCTL (form `exprOfType` FormulaTy) $
                                            failPosCTL p (expectedMsg FormulaTy)

typeCheckSentence (Models p m form)     = do unlessCTL (m `exprOfType` ModelTy) $
                                                failPosCTL p $ (expectedMsg ModelTy)
                                             unlessCTL (form `exprOfType` FormulaTy) $
                                                failPosCTL p (expectedMsg FormulaTy)

typeCheckSentence (IsValid p m _ form)  = do unlessCTL (m `exprOfType` ModelTy) $
                                                failPosCTL p (expectedMsg ModelTy)
                                             unlessCTL (form `exprOfType` FormulaTy) $
                                                failPosCTL p (expectedMsg FormulaTy)

