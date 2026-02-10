module Global
  ( Definition
  , GState (..)
  , initialState
  , Mode (..)
  , Conf (..) ) where

import Common ( VarIdent )
import Lang   ( Type, Value )

type Definition = (VarIdent, Type, Value)

-- Global State of the Monad.
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