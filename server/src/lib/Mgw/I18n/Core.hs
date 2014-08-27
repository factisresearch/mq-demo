module Mgw.I18n.Core
    ( Language(..)
    , switchLang
    , _LANG_
    )
where

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Env (getEnv)

data Language
    = German
    | English
    deriving (Eq, Ord, Show, Read)

langStr =
    unsafePerformIO $
    do lang <- getEnv "CPM_LANG"
       case lang of
         Just foo -> return foo
         _ -> return "-"
{-# NOINLINE langStr #-}

_LANG_ :: Language
_LANG_ =
    case langStr of
      "en" -> English
      _ -> German

switchLang :: a -> a -> a
switchLang ger eng =
    case _LANG_ of
      German -> ger
      English -> eng
