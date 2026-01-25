{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}


module MonadCTL (
  CTL,
  MonadCTL,
  runCTL )

where

import Global 
import Error

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO

class (MonadIO m, MonadState GState m, MonadError Error m,  MonadReader Conf m) 
  => MonadCTL m where

-- definiciones de la interfaz de la clase MONADCTL

type CTL = ReaderT Conf (StateT GState (ExceptT Error IO))

instance MonadCTL CTL

runCTL' :: CTL a -> Conf -> IO (Either Error (a, GState))
runCTL' c conf = runExceptT $ runStateT (runReaderT c conf) initialState

runCTL:: CTL a -> Conf -> IO (Either Error a)
runCTL c conf = fmap fst <$> runCTL' c conf