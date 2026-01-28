module TypeCheck where


import AST
import Common
import MonadCTL

import qualified Data.Set as Set

ofType :: MonadCTL m => Expr -> Type -> m()
ofType expr ty = do exprTy <- findType expr
                    expectType exprTy ty

expectType :: MonadCTL m => Type -> Type -> m()      
expectType ty1 ty2 = if ty1 == ty2 then return ()
                                   else failCTL "no matchean los tipos"

findType :: MonadCTL m => Expr -> m Type
findType (ModelExpr t l)   = do ofType t NodesTy
                                ofType l LabelsTy
                                return ModelTy
findType (LabelsExpr _)      = return LabelsTy
findType (NodesExpr _) = return NodesTy

findType (VarExpr var)      = getTy var

findType (FormulaExpr f)   = do let fv = Set.toList $ freeVariables f
                                mapM_ checkFormulaVarTy fv
                                return FormulaTy

    where checkFormulaVarTy :: MonadCTL m => VarIdent -> m ()
          checkFormulaVarTy var = do varTy <- getTy var
                                     expectType varTy FormulaTy

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
typeCheckSentence (Def _ ty expr)        = expr `ofType` ty 
typeCheckSentence (Export model)         = model `ofType` ModelTy
typeCheckSentence (IsValid formula)      = formula `ofType` FormulaTy
typeCheckSentence (Models model formula) = do model `ofType` ModelTy
                                              formula `ofType` FormulaTy