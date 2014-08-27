{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, ScopedTypeVariables, CPP #-}
module Mgw.Util.Logging
    (module Mgw.Util.Logging.Core
    , module Mgw.Util.Logging.LogClient
) where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Logging.Core
import Mgw.Util.Logging.LogClient
