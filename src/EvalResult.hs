module EvalResult 
  ( CheckNodeType (..)
  , ExamplePath
  , EvalResult (..) ) where

import Common ( Nodes, NodeIdent )
import Lang   ( Formula)

 -- Evaluation results of the SAT functions
data CheckNodeType = CheckInitials Nodes | CheckNode NodeIdent

type ExamplePath = [(NodeIdent, String)]

data EvalResult = EvalResult
  { evalFormula  :: Formula
  , holds        :: Bool
  , satNodes     :: Nodes
  , checkType    :: CheckNodeType
  , examplePath  :: ExamplePath
  } 
