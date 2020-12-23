{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_backlogger (
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

bindir     = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/bin"
libdir     = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/lib/x86_64-linux-ghc-8.8.4/backlogger-0.1.0.0-6vrr7k0SqFOzR1Pvz4aTY"
dynlibdir  = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/share/x86_64-linux-ghc-8.8.4/backlogger-0.1.0.0"
libexecdir = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/libexec/x86_64-linux-ghc-8.8.4/backlogger-0.1.0.0"
sysconfdir = "/media/sda1/HaskellProj/backlogger/.stack-work/install/x86_64-linux-tinfo6/c35525d957fb47afc12b89c6d679e2646ef4cbace0e6203be31a4b5fd809b246/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "backlogger_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "backlogger_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "backlogger_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "backlogger_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backlogger_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backlogger_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
