module Global where

import AST

data GState = GState {
  declarations :: [(VarIdent, Type, Expr)]
}

initialState :: GState
initialState = GState []

data Mode 
  = Interactive 
  | Eval
  | TypeCheck

data Conf = Conf {
  mode :: Mode
}

