{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_aeson (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "aeson"
version :: Version
version = Version [2,2,3,0] []

synopsis :: String
synopsis = "Fast JSON parsing and encoding"
copyright :: String
copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc."
homepage :: String
homepage = "https://github.com/haskell/aeson"
