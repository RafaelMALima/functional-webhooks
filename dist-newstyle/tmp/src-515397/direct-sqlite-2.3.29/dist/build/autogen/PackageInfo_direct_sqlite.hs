{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_direct_sqlite (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "direct_sqlite"
version :: Version
version = Version [2,3,29] []

synopsis :: String
synopsis = "Low-level binding to SQLite3.  Includes UTF8 and BLOB support."
copyright :: String
copyright = "Copyright (c) 2012 - 2014 Irene Knapp,\n2014 - 2018 Janne Hellsten,\n2018 - 2020 Sergey Bushnyak,\n2022        Joshua Chia"
homepage :: String
homepage = "https://github.com/IreneKnapp/direct-sqlite"
