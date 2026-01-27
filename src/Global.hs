module Global where

import AST
import Common

type Definition = (VarIdent, Type, Value)

data GState = GState {
  definitions :: [Definition]
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

