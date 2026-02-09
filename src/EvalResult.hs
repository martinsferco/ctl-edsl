module EvalResult where

import Common
import Lang

 -- Evaluation results of the SAT functions
data CheckNodeType = CheckInitials Nodes | CheckNode NodeIdent

type ExamplePath = [(NodeIdent, String)]

data EvalResult = EvalResult
  { evalFormula  :: Formula
  , holds        :: Bool
  , satNodes     :: Nodes
  , checkType    :: CheckNodeType
  , examplePath  :: ExamplePath
  , description  :: String
  } 
