module Error where

import Text.Parsec.Error ( ParseError )

data Error 
  = ParseErr ParseError
  | TypecheckError String
  | RuntimeError String 
  | StateError String
  | ModelError String

instance Show Error where
  show (ParseErr e)       = show e
  show (TypecheckError s) = "type check error: " ++ s
  show (RuntimeError s)   = "runtime  error: " ++ s
  show (StateError s)     = "state error: " ++ s
  show (ModelError s)     = "model error: " ++ s
