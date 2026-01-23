module Errors where

import Text.Parser.Error ( ParseError )

data Error 
  = ParseErr ParseError
  | TypeErr 
  | EvalErr