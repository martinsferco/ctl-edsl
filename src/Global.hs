module Global where


data GlobalState = GlobalState {
  lastFile :: String,
  globalDeclarations :: [Int]
}

data Configuration = Configuration {
  mode :: Mode
}

data Mode 
  = Interactive
  | Evaluation
  | TypeCheck