module Monad where

import Global 
import Errors


class (MonadIO m, MonadState GlobalState m, MonadError Errors m, MonadReader Configuration m) 
  => MonadCTL m where


    





