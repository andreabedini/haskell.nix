{-# LANGUAGE OverloadedStrings #-}

module Plan2Nix.Plan
  ( Version,
    Revision,
    URL,
    Rev,
    Plan (..),
    PkgSrc (..),
    Package (..),
    Location (..),
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Nix.Expr

type Version = Text

type Revision = Text -- Can be: rNUM, cabal file sha256, or "default"
-- See stack2nix

type URL = String

type Rev = String

data Location
  = Git URL Rev
  | HG URL Rev
  deriving (Show)

data Plan = Plan
  { packages :: HashMap Text (Maybe Package),
    extras :: HashMap Text (Maybe Package),
    components :: HashSet Text,
    compilerVersion :: Text,
    compilerPackages :: HashMap Text (Maybe Version)
  }
  deriving (Show)

data PkgSrc
  = -- | some local package (potentially overriding a package in the index as well)
    LocalPath FilePath
  | -- | One or more packages fetched from git or similar
    DVCS Location [FilePath]
  deriving (Show)

data Package = Package
  { packageVersion :: Version,
    packageRevision :: Maybe Revision,
    packageFlags :: HashMap VarName Bool,
    packageSrc :: Maybe PkgSrc
  }
  deriving (Show)
