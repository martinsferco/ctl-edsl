module Monad where

import Global 
import Errors


class (MonadIO m, MonadState GState m, MonadError Error m) => MonadCTL m where


    



newtype CTL = 

