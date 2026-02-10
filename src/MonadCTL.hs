{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MonadCTL 
  ( MonadCTL
  , CTL
  , runCTL
  , getMode
  , getLastFile
  , setLastFile
  , printCTL
  , addDef
  , getDefinitions
  , getTy
  , searchDef
  , failCTL
  , failPosCTL
  , unlessCTL ) where

import Global ( GState (..), Conf (..), Mode, Definition, initialState )
import Common ( VarIdent, Pos (..) )
import Error  ( Error (..) )
import Lang   ( Type, Value )

import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader


class (MonadIO m, MonadState GState m, MonadError Error m,  MonadReader Conf m) 
  => MonadCTL m where

-- Gets the current mode.
getMode :: MonadCTL m => m Mode
getMode = asks mode

-- Gets the last file loaded.
getLastFile :: MonadCTL m => m (Maybe String)
getLastFile = do s <- get
                 let file = lastFile s
                 if file == ""  then return Nothing
                                else return $ Just file

-- Sets the last file loaded.
setLastFile :: MonadCTL m => String -> m ()
setLastFile file = modify (\s -> s { lastFile = file })



-- Adds a new definition to the global state. In case it already 
-- exists, it replaces it (useful for the interactive mode).
addDef :: MonadCTL m => VarIdent -> Type -> Value -> m ()
addDef var ty val = do  
  modify $ \s ->
    let defs = definitions s
        defsWithoutVar = filter (\(v, _, _) -> v /= var) defs
        newDefs = (var, ty, val) : defsWithoutVar
    in s { definitions = newDefs }

-- Gets all the definition in the current global state, with their type.
getDefinitions :: MonadCTL m => m ([(VarIdent, Type)])
getDefinitions = do s <- get
                    return $ map (\(v,ty,_) -> (v,ty)) (definitions s)

-- Get the type of a certain definition.
getTy :: MonadCTL m => VarIdent -> m Type
getTy = selectDefinition (\(_, ty, _) -> ty)

-- Searchs the value of a certain definition.
searchDef :: MonadCTL m => VarIdent -> m Value
searchDef = selectDefinition (\(_, _, value) -> value)

selectDefinition :: MonadCTL m => (Definition -> a) -> VarIdent -> m a
selectDefinition select var = do
  s <- get 
  case filter (matchsName var) (definitions s) of
    def : _  -> return $ select def
    _        -> failCTL ("variable " ++ var ++ " is not defined.")

  where
    matchsName :: VarIdent -> (VarIdent, Type, Value) -> Bool
    matchsName v (varId, _, _) = v == varId


-- Prints an string.
printCTL :: MonadCTL m => String -> m ()
printCTL = liftIO . putStrLn

failPosCTL :: MonadCTL m => Pos -> String -> m a
failPosCTL p s = throwError (GeneralError p s)

failCTL :: MonadCTL m => String -> m a
failCTL s = throwError (GeneralError NoPos s)

unlessCTL :: MonadCTL m => m Bool -> m () -> m ()
unlessCTL mb action = mb >>= (\b -> unless b action)

type CTL = ReaderT Conf (StateT GState (ExceptT Error IO))

instance MonadCTL CTL

runCTL' :: CTL a -> Conf -> IO (Either Error (a, GState))
runCTL' c conf = runExceptT $ runStateT (runReaderT c conf) initialState

runCTL:: CTL a -> Conf -> IO (Either Error a)
runCTL c conf = fmap fst <$> runCTL' c conf