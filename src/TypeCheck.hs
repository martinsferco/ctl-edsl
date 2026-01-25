module TypeCheck where


import AST

import qualified Data.Set as Set

ofType :: Expr -> Type -> m()
-- encontramos el tipo de la expresion, y lo comparamos
-- contra el t 
ofType expr ty = undefined

-- esto va a tener quser mondaico porque quiero accedwer a las variables globales
findType :: Expr -> Type
-- me tengo que fijar que el primero sea trans y el segundo label
findType (ModelExpr t l)   = undefined
findType (LabelExp _)      = LabelsTy
findType (TransitionExp _) = TransitionsTy

-- me fijo en el estado que tenga tipo forumla
findType (VarExp var)      = undefined

-- me tengo que fijar que todas tengan el tipo de formula
-- findType (FormulaExpr f)   = let fv = toList $ freeVariables f
                                 

freeVariables :: Formula -> Set.Set VarIdent
freeVariables F                   = Set.empty
freeVariables T                   = Set.empty
freeVariables (Atom _)            = Set.empty
freeVariables (Not p)             = freeVariables p

freeVariables (BinaryOp _ p q)    = let varP = freeVariables p
                                        varQ = freeVariables q
                                    in Set.union varP varQ

freeVariables (UQuantifier _ p)   = freeVariables p

freeVariables (BQuantifier _ p q) = let varP = freeVariables p
                                        varQ = freeVariables q
                                    in Set.union varP varQ

freeVariables (Var var)           = Set.singleton var


-- typeCheck :: Sentence -> m() 
-- typeCheck (Def _ ty expr) = expr `ofType` ty 
-- typeCheck (Export model) = model `ofType` ModelTy
-- typeCheck (IsValid formula) = formula `ofType` FormulaTy
-- typeCheck (Models model formula) = do model `ofType` ModelTy
--                                       formula `ofType` FormulaTy

