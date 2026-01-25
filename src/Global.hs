module Global where

import AST

data GState = GState {
  declarations :: [(VarIdent, Type, Expr)]
}

data Conf = Conf {
  mode :: Mode
}

data Mode 
  = Interactive
  | Evaluation
  | TypeCheck