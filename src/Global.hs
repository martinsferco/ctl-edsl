module Global where

import Common
import Lang

type Definition = (VarIdent, Type, Value)

data GState = GState {
  definitions :: [Definition],
  lastFile :: String
}

initialState :: GState
initialState = GState [] ""

data Mode 
  = Interactive 
  | Eval
  | TypeCheck

data Conf = Conf {
  mode :: Mode
}