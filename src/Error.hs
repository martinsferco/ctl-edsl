module Error 
  ( Error (..)
  , expectedMsg
  , notOfTypeMsg
  , incorrectVarMsg ) where

import Text.Parsec.Error ( ParseError )
import PrettyPrinter     ( ppType )
import Common            ( Pos(..) )
import Lang              ( Type )

data Error 
  = ParseErr ParseError
  | GeneralError Pos String

instance Show Error where
  show (ParseErr e)      = "[Error] " ++ show e
  show (GeneralError p s) = "[Error " ++ show p ++ "] " ++ s

expectedMsg :: Type -> String
expectedMsg ty = "Expected a " ++ ppType ty ++ ", but got a different type." 

notOfTypeMsg :: String -> Type -> String
notOfTypeMsg def ty = "The expression of the definition " ++ def ++ " is not of type " ++ ppType ty 

incorrectVarMsg :: String -> Type -> String
incorrectVarMsg v ty = "The variable " ++ show v ++ " should be of type " ++ ppType ty