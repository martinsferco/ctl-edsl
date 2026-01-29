module TypeCheck where


import AST
import Common
import MonadCTL

import qualified Data.Set as Set

exprOfType :: MonadCTL m => Expr -> Type -> m()
exprOfType expr ty = do exprTy <- findTypeExpr expr
                        expectType exprTy ty

valueOfType :: MonadCTL m => Value -> Type -> m()
valueOfType value ty = do valueTy <- findTypeValue value
                          expectType valueTy ty

expectType :: MonadCTL m => Type -> Type -> m()      
expectType ty1 ty2 = if ty1 == ty2 then return ()
                                   else typeError "types do not match"

findTypeExpr :: MonadCTL m => Expr -> m Type
findTypeExpr (ModelExpr t l)   = do exprOfType t NodesTy
                                    exprOfType l LabelsTy
                                    return ModelTy
findTypeExpr (LabelsExpr _)      = return LabelsTy
findTypeExpr (NodesExpr _) = return NodesTy

findTypeExpr (VarExpr var)      = getTy var

findTypeExpr (FormulaExpr f)   = do let fv = Set.toList $ freeVariables f
                                    mapM_ checkFormulaVarTy fv
                                    return FormulaTy

    where checkFormulaVarTy :: MonadCTL m => VarIdent -> m ()
          checkFormulaVarTy var = do varTy <- getTy var
                                     expectType varTy FormulaTy


findTypeValue :: MonadCTL m => Value -> m Type
findTypeValue (Model _)   = return ModelTy
findTypeValue (Labels _)  = return LabelsTy
findTypeValue (Formula _) = return FormulaTy
findTypeValue (Nodes _)   = return NodesTy

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
typeCheckSentence (Def _ ty expr)        = expr `exprOfType` ty 
typeCheckSentence (Export model)         = model `exprOfType` ModelTy
typeCheckSentence (IsSatis formula)      = formula `exprOfType` FormulaTy
typeCheckSentence (Models model formula) = do model `exprOfType` ModelTy
                                              formula `exprOfType` FormulaTy
typeCheckSentence (IsValid model _ form) = do model `exprOfType` ModelTy
                                              form `exprOfType` FormulaTy