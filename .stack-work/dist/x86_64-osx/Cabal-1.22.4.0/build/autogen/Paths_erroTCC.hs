module Paths_erroTCC (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/eric/Desktop/caue/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/bin"
libdir     = "/Users/eric/Desktop/caue/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/lib/x86_64-osx-ghc-7.10.2/erroTCC-0.1-CGbL84zcURG8Gk4IG2GuKS"
datadir    = "/Users/eric/Desktop/caue/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/share/x86_64-osx-ghc-7.10.2/erroTCC-0.1"
libexecdir = "/Users/eric/Desktop/caue/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/libexec"
sysconfdir = "/Users/eric/Desktop/caue/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "erroTCC_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "erroTCC_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "erroTCC_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "erroTCC_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "erroTCC_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
