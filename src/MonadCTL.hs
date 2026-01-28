{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}

module MonadCTL  where

import Error
import Common
import AST
import Global

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO




class (MonadIO m, MonadState GState m, MonadError Error m,  MonadReader Conf m) 
  => MonadCTL m where


type CTL = ReaderT Conf (StateT GState (ExceptT Error IO))


getMode :: MonadCTL m => m Mode
getMode = asks mode

printCTL :: MonadCTL m => String -> m ()
printCTL = liftIO . putStrLn

addDef :: MonadCTL m => VarIdent -> Type -> Value -> m ()
addDef var ty val = do  
  notExists var
  modify $ \state ->
    state { definitions = (var, ty, val) : definitions state }

  where
    notExists :: MonadCTL m => VarIdent -> m ()
    notExists var = do
      s <- get
      case filter (matchsName var) (definitions s) of 
        _ : _ -> failCTL "error: existe la variable"
        []    -> return ()


typingContext :: MonadCTL m => m ([(VarIdent, Type)])
typingContext = do state <- get
                   return $ map (\(v,ty,_) -> (v,ty)) (definitions state)

getTy :: MonadCTL m => VarIdent -> m Type
getTy = selectDefinition (\(_, ty, _) -> ty)

searchDef :: MonadCTL m => VarIdent -> m Value
searchDef = selectDefinition (\(_, _, value) -> value)

selectDefinition :: MonadCTL m => (Definition -> a) -> VarIdent -> m a
selectDefinition select var = do
  state <- get 
  case filter (matchsName var) (definitions state) of
    def : _  -> return $ select def
    _        -> failCTL "error: no existe la variable"

matchsName :: VarIdent -> (VarIdent, Type, Value) -> Bool
matchsName var (varId, _, _) = var == varId

failCTL :: MonadCTL m => String -> m a
failCTL = undefined

instance MonadCTL CTL

runCTL' :: CTL a -> Conf -> IO (Either Error (a, GState))
runCTL' c conf = runExceptT $ runStateT (runReaderT c conf) initialState

runCTL:: CTL a -> Conf -> IO (Either Error a)
runCTL c conf = fmap fst <$> runCTL' c conf