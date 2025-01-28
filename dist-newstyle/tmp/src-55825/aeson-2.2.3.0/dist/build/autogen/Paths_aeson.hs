{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aeson (
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
version = Version [2,2,3,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/bin"
libdir     = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/lib"
dynlibdir  = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/lib"
datadir    = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/share"
libexecdir = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/libexec"
sysconfdir = "/home/filk/.cabal/store/ghc-9.4.8/aeson-2.2.3.0-154c2a5db26d97a6e69fa87e8c3ba0534ad101eaac7d52f2a50872f4edcd4f8d/etc"

getBinDir     = catchIO (getEnv "aeson_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aeson_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aeson_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aeson_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aeson_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aeson_sysconfdir") (\_ -> return sysconfdir)



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
