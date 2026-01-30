module Error where

import Text.Parsec.Error ( ParseError )
import Common ( Pos )

data Error 
  = ParseErr ParseError
  | GeneralError Pos String

  
instance Show Error where
  show (ParseErr e)     = show e
  show (GeneralError p s) = show p ++ show s