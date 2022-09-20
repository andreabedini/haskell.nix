{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Plan2Nix
  ( doPlan2Nix,
    planexpr,
    plan2nix,
  )
where

import Cabal2Nix (cabal2nix, cabalFilePkgName)
import Cabal2Nix qualified as C2N
import Cabal2Nix.Util (bindPath, quoted)
import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Aeson (Value, eitherDecodeFileStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    isJust,
    mapMaybe,
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Traversable (for)
import Distribution.Nixpkgs.Fetch (DerivationSource (..), Hash (..), Source (..), fetch)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils (shortRelativePath)
import Distribution.Types.Version (Version)
import Lens.Micro ((^.), (^?))
import Lens.Micro.Aeson
  ( AsValue (_Array, _Bool, _Object, _String),
    key,
  )
import Nix.Expr
import Nix.Pretty (prettyNix)
import Plan2Nix.CLI (Args (..))
import Plan2Nix.Cache (appendCache, cacheHits)
import Plan2Nix.Plan (Location (..), Package (..), PkgSrc (..), Plan (..))
import Plan2Nix.Project (findCabalFiles)
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.FilePath (dropFileName, takeDirectory, (<.>), (</>))
import System.IO (IOMode (..), hClose, openFile)

doPlan2Nix :: Args -> IO ()
doPlan2Nix args = do
  let pkgsNix = argOutputDir args </> "pkgs.nix"
      defaultNix = argOutputDir args </> "default.nix"
  pkgs <- planexpr args
  writeDoc pkgsNix (prettyNix pkgs)
  unlessM (doesFileExist defaultNix) $
    writeFile defaultNix defaultNixContents

planexpr :: Args -> IO NExpr
planexpr args =
  do
    evalue <- eitherDecodeFileStrict (argPlanJSON args)
    case evalue of
      Left e -> error (show e)
      Right value -> plan2nix args $ value2plan value

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  do
    handle <- openFile file WriteMode
    hPutDoc handle doc
    hClose handle

plan2nix :: Args -> Plan -> IO NExpr
plan2nix
  Args {argCacheFile, argCabalProject, argDetailLevel, argOutputDir}
  Plan {packages, extras, components, compilerVersion, compilerPackages} = do
    -- TODO: this is an aweful hack and expects plan-to-nix to be
    -- called from the toplevel project directory.
    cwd <- getCurrentDirectory

    extrasNix <- do
      tmp <- for (mapMaybe packageSrc $ catMaybes $ toList extras) $
        \case
          LocalPath folder ->
            do
              cabalFiles <- findCabalFiles folder
              for cabalFiles $ \cabalFile -> do
                let pkg = cabalFilePkgName cabalFile
                    nix = ".plan.nix" </> pkg <.> "nix"
                    nixFile = argOutputDir </> nix
                    src = Just . C2N.Path $ relPath </> ".." </> shortRelativePath cwd folder
                createDirectoryIfMissing True (takeDirectory nixFile)
                writeDoc nixFile . prettyNix =<< cabal2nix True argDetailLevel src cabalFile
                return $ T.pack pkg $= mkPath False nix
          DVCS (Git url rev) subdirs -> do
            tmp <- for subdirs $ \subdir -> do
              hits <- cacheHits argCacheFile url rev subdir
              case hits of
                [] -> do
                  x <- fetch (\dir -> cabalFromPath url rev subdir $ dir </> subdir) (Source url rev UnknownHash)
                  case x of
                    (Just (DerivationSource {..}, genBindings)) -> genBindings derivHash
                    _ -> return []
                hits ->
                  for hits $ \(pkg, nix) ->
                    return $ T.pack pkg $= mkPath False nix
            return $ concat tmp
          _ -> return []
      return $ mkNonRecSet $ concat tmp

    let flags =
          concatMap
            ( \case
                (name, Just (Package _v _r f _)) -> flags2nix name f
                _ -> []
            )
            $ Map.toList extras

        -- Set the `planned` option for all components in the plan.
        planned =
          map
            ( \name ->
                name <> ".planned"
                  $= ("lib" @. "mkOverride" @@ mkInt 900 @@ mkBool True)
            )
            $ Set.toList components

    return $
      mkNonRecSet
        [ "pkgs"
            $= ( "hackage"
                   ==> mkNonRecSet
                     [ "packages"
                         $= mkNonRecSet (do (t, mp) <- Map.toList packages; bind (quoted t) mp),
                       "compiler"
                         $= mkNonRecSet
                           [ "version" $= mkStr compilerVersion,
                             "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion),
                             "packages" $= mkNonRecSet [bind' (quoted t) mp | (t, mp) <- Map.toList compilerPackages]
                           ]
                     ]
               ),
          "extras" $= ("hackage" ==> mkNonRecSet ["packages" $= extrasNix]),
          "modules"
            $= mkList
              [ mkParamset [("lib", Nothing)] True ==> mkNonRecSet ["packages" $= mkNonRecSet flags],
                mkParamset [("lib", Nothing)] True ==> mkNonRecSet ["packages" $= mkNonRecSet planned]
              ]
        ]
    where
      bind :: Text -> Maybe Package -> [Binding NExpr]
      bind pkg (Just Package {packageVersion, packageRevision, packageFlags}) =
        let verExpr = (mkSym "hackage" @. pkg) @. quoted packageVersion
            revExpr = (verExpr @. "revisions") @. maybe "default" quoted packageRevision
            flagBindings =
              Map.foldrWithKey
                (\fname val acc -> bindPath (VarName pkg :| ["flags", fname]) (mkBool val) : acc)
                []
                packageFlags
         in revBinding pkg revExpr : flagBindings
      bind pkg Nothing = [revBinding pkg mkNull]

      revBinding :: Text -> NExpr -> Binding NExpr
      revBinding pkg revExpr = bindPath (VarName pkg :| ["revision"]) revExpr

      bind' :: Text -> Maybe Text -> Binding NExpr
      bind' pkg ver = pkg $= maybe mkNull mkStr ver

      relPath = shortRelativePath argOutputDir (dropFileName argCabalProject)

      cabalFromPath ::
        String -> -- URL
        String -> -- Revision
        FilePath -> -- Subdir
        FilePath -> -- Local Directory
        MaybeT IO (String -> IO [Binding NExpr])
      cabalFromPath url rev subdir path = do
        d <- liftIO $ doesDirectoryExist path
        unless d $ fail ("not a directory: " ++ path)
        cabalFiles <- liftIO $ findCabalFiles path
        return $ \sha256 ->
          for cabalFiles $ \cabalFile -> do
            let pkg = cabalFilePkgName cabalFile
                nix = ".plan.nix" </> pkg <.> "nix"
                nixFile = argOutputDir </> nix
                subdir' =
                  if subdir == "."
                    then Nothing
                    else Just subdir
                src = Just $ C2N.Git url rev (Just sha256) subdir'
            createDirectoryIfMissing True (takeDirectory nixFile)
            writeDoc nixFile . prettyNix =<< cabal2nix True argDetailLevel src cabalFile
            liftIO $ appendCache argCacheFile url rev subdir sha256 pkg nix
            return $ T.pack pkg $= mkPath False nix

-- | Converts the project flags for a package flags into @{ packageName = { flags = { flagA = BOOL; flagB = BOOL; }; }; }@
flags2nix :: Text -> HashMap VarName Bool -> [Binding NExpr]
flags2nix pkgName pkgFlags =
  [ quoted pkgName
      $= mkNonRecSet
        -- `mkOverride 900` is used here so that the default values will be replaced (they are 1000).
        -- Values without a priority are treated as 100 and will replace these ones.
        [ "flags"
            $= mkNonRecSet
              [ quoted flag $= ("lib" @. "mkOverride" @@ mkInt 900 @@ mkBool val)
                | (VarName flag, val) <- Map.toList pkgFlags
              ]
        ]
  ]

flagsForPkg :: Value -> HashMap VarName Bool
flagsForPkg pkg =
  Map.fromList . fmap (\(k, v) -> (VarName (Key.toText k), v))
    . KeyMap.toList
    $ KeyMap.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object

value2plan :: Value -> Plan
value2plan plan = Plan {packages, components, extras, compilerVersion, compilerPackages}
  where
    packages = fmap Just $
      filterInstallPlan $ \pkg -> case ( pkg ^. key "type" . _String,
                                         pkg ^. key "style" . _String
                                       ) of
        -- source-repo packages will be included in `extras`.  We do not need them
        -- in `packages` as well (this could lead to attribute not found errors looking
        -- for them in hackage).
        (_, _)
          | pkg ^. key "pkg-src" . key "type" . _String == "source-repo" ->
            Nothing
        (_, "global") ->
          Just $
            Package
              { packageVersion = pkg ^. key "pkg-version" . _String,
                packageRevision = Nothing,
                packageFlags = flagsForPkg pkg,
                packageSrc = Nothing
              }
        (_, "inplace") ->
          Just $
            Package
              { packageVersion = pkg ^. key "pkg-version" . _String,
                packageRevision = Nothing,
                packageFlags = flagsForPkg pkg,
                packageSrc = Nothing
              }
        -- Until we figure out how to force Cabal to reconfigure just about any package
        -- this here might be needed, so that we get the pre-existing packages as well.
        -- Or we would have to plug in our very custom minimal pkg-db as well.
        --
        -- The issue is that cabal claims anything in the package db as pre-existing and
        -- wants to reuse it if possible.
        ("pre-existing", _) ->
          Just $
            Package
              { packageVersion = pkg ^. key "pkg-version" . _String,
                packageRevision = Nothing,
                packageFlags = Map.empty,
                packageSrc = Nothing
              }
        _ -> Nothing

    extras = fmap Just $
      filterInstallPlan $ \pkg -> case ( pkg ^. key "style" . _String,
                                         pkg ^. key "pkg-src" . key "type" . _String
                                       ) of
        ("local", "local") ->
          Just $
            Package
              { packageVersion = pkg ^. key "pkg-version" . _String,
                packageRevision = Nothing,
                packageFlags = flagsForPkg pkg,
                packageSrc = Just . LocalPath . Text.unpack $ pkg ^. key "pkg-src" . key "path" . _String
              }
        (_, "source-repo") ->
          Just $
            Package
              { packageVersion = pkg ^. key "pkg-version" . _String,
                packageRevision = Nothing,
                packageFlags = flagsForPkg pkg,
                packageSrc =
                  Just . flip DVCS [Text.unpack $ fromMaybe "." $ pkg ^? key "pkg-src" . key "source-repo" . key "subdir" . _String] $
                    Git
                      (Text.unpack $ pkg ^. key "pkg-src" . key "source-repo" . key "location" . _String)
                      (Text.unpack $ pkg ^. key "pkg-src" . key "source-repo" . key "tag" . _String)
              }
        _ -> Nothing

    compilerVersion = Text.dropWhile (not . isDigit) $ plan ^. key "compiler-id" . _String
    compilerPackages = fmap Just $
      filterInstallPlan $ \pkg ->
        if isJust (pkg ^? key "style" . _String)
          then Nothing
          else Just $ pkg ^. key "pkg-version" . _String

    filterInstallPlan :: (Value -> Maybe b) -> HashMap Text b
    filterInstallPlan f =
      fmap snd
        .
        -- If the same package occurs more than once, choose the latest
        Map.fromListWith (\a b -> if parseVersion (fst a) > parseVersion (fst b) then a else b)
        $ mapMaybe (\pkg -> (,) (pkg ^. key "pkg-name" . _String) . (pkg ^. key "pkg-version" . _String,) <$> f pkg) $
          toList (plan ^. key "install-plan" . _Array)

    parseVersion :: Text -> Version
    parseVersion s = fromMaybe (error $ "Unable to parse version " <> show s) . simpleParsec $ Text.unpack s

    -- Set of components that are included in the plan.
    components :: HashSet Text
    components =
      Set.fromList $
        concatMap
          ( \pkg ->
              let pkgName = pkg ^. key "pkg-name" . _String
                  nixComponentAttr = Text.pack . componentNameToHaskellNixAttr pkgName . Text.unpack
               in map ((quoted pkgName <> ".components.") <>) $
                    if pkg ^. key "type" . _String == "pre-existing"
                      then ["library"]
                      else -- If a `components` attribute exists then the keys of that are the component names.
                      -- If it does not exist then look for `component-name` instead.

                        maybe
                          [nixComponentAttr $ pkg ^. key "component-name" . _String]
                          (map (nixComponentAttr . Key.toText) . KeyMap.keys)
                          (pkg ^? key "components" . _Object)
          )
          $ toList (plan ^. key "install-plan" . _Array)

    -- Convert a cabal style component name to the haskell.nix attribute path.
    componentNameToHaskellNixAttr :: Text -> String -> String
    componentNameToHaskellNixAttr pkgName n =
      case span (/= ':') n of
        ("setup", "") -> "setup"
        ("lib", "") -> "library"
        (prefix, ':' : rest) -> componentPrefixToHaskellNix prefix <> "." <> quoted rest
        _ -> error ("unknown component name format " <> show n <> " for package " <> show pkgName)

    componentPrefixToHaskellNix :: String -> String
    componentPrefixToHaskellNix "lib" = "sublibs"
    componentPrefixToHaskellNix "flib" = "foreignlibs"
    componentPrefixToHaskellNix "exe" = "exes"
    componentPrefixToHaskellNix "test" = "tests"
    componentPrefixToHaskellNix "bench" = "benchmarks"
    componentPrefixToHaskellNix x = error ("unknown component prefix " <> x)

defaultNixContents :: String
defaultNixContents =
  unlines
    [ "{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz",
      ", haskellNix ? import haskellNixSrc {}",
      ", nixpkgs ? haskellNix.sources.nixpkgs }:",
      "",
      "let",
      "  pkgs = import nixpkgs haskellNix.nixpkgsArgs;",
      "",
      "  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {",
      "    plan-pkgs = import ./pkgs.nix;",
      "    pkg-def-extras = [];",
      "    modules = [];",
      "  };",
      "",
      "in",
      "  pkgSet.config.hsPkgs"
    ]
