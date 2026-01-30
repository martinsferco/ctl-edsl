module Error where

import Text.Parsec.Error ( ParseError )
import Common ( Pos )

data Error 
  = ParseErr ParseError
  | GeneralError Pos String

  
instance Show Error where
  show (ParseErr e)      = "[Error] " ++ show e
  show (GeneralError p s) = "[Error " ++ show p ++ "] " ++ s