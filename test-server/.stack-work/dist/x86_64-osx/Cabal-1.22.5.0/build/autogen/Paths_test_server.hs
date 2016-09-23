module Paths_test_server (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/philip/Documents/haskell/test-server/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/bin"
libdir     = "/Users/philip/Documents/haskell/test-server/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/lib/x86_64-osx-ghc-7.10.3/test-server-0.1.0.0-KFhR5m0owdK0zh34EGb7D6"
datadir    = "/Users/philip/Documents/haskell/test-server/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/share/x86_64-osx-ghc-7.10.3/test-server-0.1.0.0"
libexecdir = "/Users/philip/Documents/haskell/test-server/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/libexec"
sysconfdir = "/Users/philip/Documents/haskell/test-server/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "test_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "test_server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "test_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "test_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "test_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
