{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_temp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "c:\\haskell\\cabal\\bin"
libdir     = "c:\\haskell\\cabal\\x86_64-windows-ghc-8.10.7\\temp-0.1.0.0-inplace-temp"
dynlibdir  = "c:\\haskell\\cabal\\x86_64-windows-ghc-8.10.7"
datadir    = "c:\\haskell\\cabal\\x86_64-windows-ghc-8.10.7\\temp-0.1.0.0"
libexecdir = "c:\\haskell\\cabal\\temp-0.1.0.0-inplace-temp\\x86_64-windows-ghc-8.10.7\\temp-0.1.0.0"
sysconfdir = "c:\\haskell\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "temp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "temp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "temp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "temp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "temp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "temp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
