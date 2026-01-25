module Error where

import Text.Parsec.Error ( ParseError )

data Error 
  = ParseErr ParseError
  | TypeErr 
  | EvalErr
  deriving (Show, Eq)