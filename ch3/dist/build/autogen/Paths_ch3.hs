{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ch3 (
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

bindir     = "/Users/scott/Library/Haskell/bin"
libdir     = "/Users/scott/Library/Haskell/ghc-8.0.2-x86_64/lib/ch3-0.1.0.0"
dynlibdir  = "/Users/scott/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/scott/Library/Haskell/share/ghc-8.0.2-x86_64/ch3-0.1.0.0"
libexecdir = "/Users/scott/Library/Haskell/libexec"
sysconfdir = "/Users/scott/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ch3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ch3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ch3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ch3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ch3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ch3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
