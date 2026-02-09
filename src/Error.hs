module Error where

import Text.Parsec.Error ( ParseError )
import Lang ( Type )
import Common ( Pos(..) )

data Error 
  = ParseErr ParseError
  | GeneralError Pos String

instance Show Error where
  show (ParseErr e)      = "[Error] " ++ show e
  show (GeneralError p s) = "[Error " ++ show p ++ "] " ++ s

instance Semigroup Error where
  e <> _ = e

instance Monoid Error where
  mempty = GeneralError NoPos ""

expectedMsg :: Type -> String
expectedMsg ty = "Expected a " ++ show ty ++ ", but got a different type." 

notOfTypeMsg :: String -> Type -> String
notOfTypeMsg def ty = "The expression of the definition " ++ def ++ " is not of type " ++ show ty 

incorrectVarMsg :: String -> Type -> String
incorrectVarMsg v ty = "The variable " ++ show v ++ " should be of type " ++ show ty