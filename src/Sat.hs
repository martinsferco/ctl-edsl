module Sat where

-- import AST
-- import qualified Data.Set as Set

-- -- Capaz lo hacemos monadico por el MEME

-- sat :: TSystem -> Formula -> Set.Set NodeIdent
-- sat = sat' . transform
--   where 
--     sat' _  F = Set.empty
--     sat' m (Atom a)             = undefined
--     sat' m (Not p)              = undefined
--     sat' m (BinaryOp And p q)   = undefined
--     sat' m (UQuantifier EC p)   = undefined
--     sat' m (UQuantifier AC p)   = undefined
--     sat' m (UQuantifier AR p)   = undefined
--     sat' m (BQuantifier EU p q) = undefined
--     sat' _ _  = error "It should be transformed already"

-- counterExample ::  TSystem -> Formula
-- counterExample = undefined


-- transform :: Formula -> Formula
-- transform F = F
-- transform T = (Not F)
-- transform v@(Var _) = v -- Capaz buscar la variable????
-- transform a@(Atom _) = a
-- transform (BinaryOp And p q) = BinaryOp And (transform p) (transform q)
-- transform (BinaryOp Or p q) = let tp = Not (transform p)
--                                   tq = Not (transform q)
--                               in Not (BinaryOp And tp tq)
  
-- transform (BinaryOp Implies p q) = let tp = transform p
--                                        tq = Not (transform q)
--                                    in Not (BinaryOp And tp tq)
  
-- transform (UQuantifier AC p) = undefined
-- transform (UQuantifier AC p) = undefined
-- transform (UQuantifier AC p) = undefined
-- transform (UQuantifier AC p) = undefined
-- transform (UQuantifier AC p) = undefined
-- transform (UQuantifier AC p) = undefined
-- transform (BQuantifier AU p q) = undefined
-- transform (BQuantifier EU p q) = undefined


-- inev :: TSystem -> Set.Set NodeIdent -> Set.Set NodeIdent
-- inev nodes = undefined

-- exUntil :: TSystem -> Set.Set NodeIdent -> Set.Set NodeIdent
-- exUntil m  = undefined

-- forAllUntil :: TSystem -> Set.Set NodeIdent -> Set.Set NodeIdent
-- forAllUntil = undefined

-- -- filtro todos los nodos del sistema, para los cuales existe 
-- -- una transicion dentro de NODES
-- preExist :: TSystem -> Set.Set NodeIdent -> Set.Set NodeIdent
-- preExist ts nodes = undefined

-- -- filtro todos los nodos del sistema, para los cuales todas 
-- -- sus transiciones dentro de NODES
-- preForAll :: TSystem -> Set.Set NodeIdent -> Set.Set NodeIdent
-- preForAll ts nodes = undefined