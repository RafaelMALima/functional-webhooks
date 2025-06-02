{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_direct_sqlite (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [2,3,29] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/bin"
libdir     = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/lib"
dynlibdir  = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/lib"
datadir    = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/share"
libexecdir = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/libexec"
sysconfdir = "/home/lemon/.cabal/store/ghc-9.6.7/direct-sqlite-2.3.29-5f41e0f562e653eac677d97219c9c591eb4d1f0be51f874e590a00c9b451271b/etc"

getBinDir     = catchIO (getEnv "direct_sqlite_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "direct_sqlite_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "direct_sqlite_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "direct_sqlite_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "direct_sqlite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "direct_sqlite_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
