{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( CabalDirLayout,
    DistDirLayout (..),
    defaultDistDirLayout,
    mkCabalDirLayout,
  )
import Distribution.Client.GlobalFlags
import Distribution.Client.HashValue (HashValue, hashValue, showHashValue)
import Distribution.Client.HttpUtils (configureTransport)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.PackageHash
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
  ( rebuildInstallPlan,
    rebuildProjectConfig,
  )
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Setup
import Distribution.Client.Types.ConfiguredId
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo
import qualified Distribution.Client.Utils.Json as J
import Distribution.Compat.Directory (makeAbsolute)
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.Simple.BuildPaths (buildInfoPref, dllExtension, exeExtension)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps
import Distribution.System (Platform (Platform))
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Types.PackageId
import Distribution.Types.SourceRepo
import Distribution.Types.Version (mkVersion)
import Distribution.Verbosity (moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import Network.URI (URI (uriPath), relativeTo)
import Nix
import System.Environment (getArgs)
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  case commandParseArgs cmdUI True args of
    CommandHelp help -> putStrLn (help "something")
    CommandList opts -> putStrLn $ "commandList" ++ show opts
    CommandErrors errs -> putStrLn $ "commandErrors: " ++ show errs
    CommandReadyToGo (mkflags, _commandParse) ->
      let globalFlags = defaultGlobalFlags
          flags@NixStyleFlags {configFlags} = mkflags (commandDefaultFlags cmdUI)
          verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
       in installPlanAction verbosity cliConfig

cmdUI :: CommandUI (NixStyleFlags ())
cmdUI =
  CommandUI
    { commandName = "",
      commandSynopsis = "Makes an install-plan",
      commandUsage = ("Usage: " ++),
      commandDescription = Nothing,
      commandNotes = Nothing,
      commandDefaultFlags = defaultNixStyleFlags (),
      commandOptions = nixStyleOptions (const [])
    }

installPlanAction :: Verbosity.Verbosity -> ProjectConfig -> IO ()
installPlanAction verbosity cliConfig = do
  let ProjectConfigShared {projectConfigDistDir, projectConfigProjectFile} = projectConfigShared cliConfig
  let mProjectFile = flagToMaybe projectConfigProjectFile
  let mdistDirectory = flagToMaybe projectConfigDistDir

  Right projectRoot <- findProjectRoot Nothing mProjectFile

  let distDirLayout = defaultDistDirLayout projectRoot mdistDirectory

  httpTransport <- configureTransport verbosity mempty mempty

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      -- more verbose here to list the project files which have affected
      -- the project configuration with no extra options
      (moreVerbose verbosity)
      httpTransport
      distDirLayout
      cliConfig

  cabalDirLayout <- cabalDirLayoutFromProjectConfig projectConfig

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  print $ prettyNix $ makeNixPlan distDirLayout elaboratedPlan elaboratedSharedConfig

--Cabal.notice verbosity $ "Writing plan.json to " ++ distProjectCacheFile distDirLayout "plan.json"
--writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

--let cabalFreezeFile = distProjectFile distDirLayout "freeze"
--Cabal.notice verbosity $ "Wrote freeze file to " ++ cabalFreezeFile
--writeProjectConfigFile cabalFreezeFile projectConfig

--let cabalFilesDir = distDirectory distDirLayout </> "cabal-files"
--Cabal.createDirectoryIfMissingVerbose verbosity True cabalFilesDir
--Cabal.notice verbosity $ "Writing cabal files to " ++ cabalFilesDir

--let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]

--for_ ecps $
--  \ElaboratedConfiguredPackage
--     { elabPkgSourceId,
--       elabPkgDescriptionOverride
--     } -> do
--      let pkgFile = cabalFilesDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
--      for_ elabPkgDescriptionOverride $ \pkgTxt -> do
--        Cabal.info verbosity $ "Writing cabal file for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
--        BSL.writeFile pkgFile pkgTxt

cabalDirLayoutFromProjectConfig :: ProjectConfig -> IO CabalDirLayout
cabalDirLayoutFromProjectConfig
  ProjectConfig
    { projectConfigBuildOnly = ProjectConfigBuildOnly {projectConfigLogsDir},
      projectConfigShared = ProjectConfigShared {projectConfigStoreDir}
    } = do
    cabalDir <- getCabalDir
    let mlogsDir = flagToMaybe projectConfigLogsDir
    mstoreDir <- sequenceA $ makeAbsolute <$> flagToMaybe projectConfigStoreDir
    return $ mkCabalDirLayout cabalDir mstoreDir mlogsDir

makeNixPlan :: DistDirLayout -> ElaboratedInstallPlan -> ElaboratedSharedConfig -> NExpr
makeNixPlan distDirLayout elaboratedInstallPlan elaboratedSharedConfig =
  "self" ==> mkNonRecSet (map planPackageToNix $ InstallPlan.toList elaboratedInstallPlan)
  where
    -- mkNonRecSet
    --   J.object
    --   [ "cabal-version" J..= jdisplay cabalInstallVersion,
    --     "cabal-lib-version" J..= jdisplay cabalVersion,
    --     "compiler-id"
    --       J..= (J.String . showCompilerId . pkgConfigCompiler)
    --         elaboratedSharedConfig,
    --     "os" J..= jdisplay os,
    --     "arch" J..= jdisplay arch,
    --     "install-plan" J..= installPlanToNix elaboratedInstallPlan
    --   ]

    planPackageToNix :: ElaboratedPlanPackage -> Binding NExpr
    planPackageToNix pkg =
      case pkg of
        InstallPlan.PreExisting ipi -> installedPackageInfoToNix ipi
        InstallPlan.Configured elab -> elaboratedPackageToNix False elab
        InstallPlan.Installed elab -> elaboratedPackageToNix True elab

    installedPackageInfoToNix :: InstalledPackageInfo -> Binding NExpr
    installedPackageInfoToNix
      InstalledPackageInfo
        { installedUnitId,
          depends,
          sourcePackageId = PackageIdentifier {pkgName, pkgVersion}
        } =
        quoted (prettyShowText installedUnitId)
          $= mkNonRecSet
            [ "type" $= "pre-existing",
              "pkg-name" $= mkStr (prettyShowText pkgName),
              "pkg-version" $= mkStr (prettyShowText pkgVersion),
              "depends" $= mkList (map (\a -> "self" @. quoted (prettyShowText a)) depends)
            ]

    elaboratedPackageToNix :: Bool -> ElaboratedConfiguredPackage -> Binding NExpr
    elaboratedPackageToNix
      isInstalled
      ElaboratedConfiguredPackage
        { elabUnitId,
          elabPkgSourceId = PackageIdentifier {pkgName, pkgVersion},
          elabFlagAssignment,
          elabPkgSourceLocation,
          elabPkgSourceHash
        } =
        T.pack (quoted $ prettyShow elabUnitId)
          $= mkNonRecSet
            [ "type" $= mkStr (if isInstalled then "installed" else "configured"),
              "pkg-name" $= mkStr (prettyShowText pkgName),
              "pkg-version" $= mkStr (prettyShowText pkgVersion),
              "flags" $= flagAssignmentToNix elabFlagAssignment,
              "pkg-src" $= packageLocationToNix elabPkgSourceLocation elabPkgSourceHash
            ]

packageLocationToNix :: PackageLocation (Maybe FilePath) -> Maybe PackageSourceHash -> NExpr
packageLocationToNix RepoTarballPackage {} Nothing = error "cannot"
packageLocationToNix RemoteSourceRepoPackage {} Nothing = error "cannot"
packageLocationToNix
  (RepoTarballPackage repo pkgId _)
  (Just srcHash) =
    case repo of
      (RepoLocalNoIndex lr s) -> undefined
      (RepoRemote rr s) -> undefined
      (RepoSecure RemoteRepo {remoteRepoURI} s) ->
        let uri = (remoteRepoURI {uriPath = uriPath remoteRepoURI </> "packages" </> prettyShow pkgId <.> ".tar.gz"})
         in fetch uri srcHash
packageLocationToNix (LocalUnpackedPackage local) _ =
  mkNonRecSet
    [ "type" $= mkStr "local",
      "path" $= mkStr (T.pack local)
    ]
packageLocationToNix (LocalTarballPackage local) _ =
  mkNonRecSet
    [ "type" $= mkStr "local-tar",
      "path" $= mkStr (T.pack local)
    ]
packageLocationToNix (RemoteTarballPackage uri _) _ =
  mkNonRecSet
    [ "type" $= mkStr "remote-tar",
      "uri" $= mkStr (showText uri)
    ]
packageLocationToNix (RemoteSourceRepoPackage srcRepo _) (Just srcHash) =
  case srcRepo of
    SourceRepositoryPackage (KnownRepoType Git) srpLocation srpTag srpBranch srpSubdir _ ->
      "makeSdistFromGitSourceRepo"
        @@ mkNonRecSet
          [ "location" $= mkStr (T.pack srpLocation),
            "branch" $= maybe mkNull (mkStr . T.pack) srpBranch,
            "tag" $= maybe mkNull (mkStr . T.pack) srpTag,
            "subdir" $= maybe mkNull (mkStr . T.pack) srpSubdir,
            "sha256" $= mkStr (T.pack $ showHashValue srcHash)
          ]
    SourceRepositoryPackage {srpType} ->
      error $ show srpType ++ " not supported"

fetch :: URI -> HashValue -> NExpr
fetch uri hash =
  "fetchTarball"
    @@ mkNonRecSet
      [ "url" $= mkStr (showText uri),
        "sha256" $= mkStr (T.pack $ showHashValue hash)
      ]

showText :: Show a => a -> Text
showText = T.pack . show

prettyShowText :: Pretty a => a -> Text
prettyShowText = T.pack . prettyShow

flagAssignmentToNix :: PD.FlagAssignment -> NExpr
flagAssignmentToNix flagAssignment =
  mkNonRecSet [prettyShowText fn $= mkBool fv | (fn, fv) <- PD.unFlagAssignment flagAssignment]

-- J.object $
--   [ "type"
--       J..= J.String
--         ( if isInstalled
--             then "installed"
--             else "configured"
--         ),
--     "id" J..= (J.String . prettyShow . elabUnitId) elab,
--     "pkg-name" J..= (J.String . prettyShow . pkgName . elabPkgSourceId) elab,
--     "pkg-version" J..= (J.String . prettyShow . pkgVersion . elabPkgSourceId) elab,
--     "flags"
--       J..= J.object
--         [ PD.unFlagName fn J..= v
--           | (fn, v) <- PD.unFlagAssignment (elabFlagAssignment elab)
--         ],
--     "style" J..= J.String (style2str (elabLocalToProject elab) (elabBuildStyle elab)),
--     "pkg-src" J..= packageLocationToNix (elabPkgSourceLocation elab)
--   ]
--     ++ [ "pkg-cabal-sha256" J..= J.String (showHashValue hash)
--          | Just hash <- [fmap hashValue (elabPkgDescriptionOverride elab)]
--        ]
--     ++ [ "pkg-src-sha256" J..= J.String (showHashValue hash)
--          | Just hash <- [elabPkgSourceHash elab]
--        ]
--     ++ ( case elabBuildStyle elab of
--            BuildInplaceOnly ->
--              ("dist-dir" J..= J.String dist_dir) : [buildInfoFileLocation]
--            BuildAndInstall ->
--              -- TODO: install dirs?
--              []
--        )
--     ++ case elabPkgOrComp elab of
--       ElabPackage pkg ->
--         let components =
--               J.object $
--                 [ comp2str c
--                     J..= J.object
--                       ( [ "depends" J..= map (J.String . prettyShow . confInstId) ldeps,
--                           "exe-depends" J..= map (J.String . prettyShow . confInstId) edeps
--                         ]
--                           ++ bin_file c
--                       )
--                   | (c, (ldeps, edeps)) <-
--                       ComponentDeps.toList $
--                         ComponentDeps.zip
--                           (pkgLibDependencies pkg)
--                           (pkgExeDependencies pkg)
--                 ]
--          in ["components" J..= components]
--       ElabComponent comp ->
--         [ "depends" J..= map (J.String . prettyShow . confInstId) (elabLibDependencies elab),
--           "exe-depends" J..= map (J.String . prettyShow) (elabExeDependencies elab),
--           "component-name" J..= J.String (comp2str (compSolverName comp))
--         ]
--           ++ bin_file (compSolverName comp)
-- where
--   buildInfoFileLocation :: J.Pair
--   buildInfoFileLocation
--     | elabSetupScriptCliVersion elab < mkVersion [3, 7, 0, 0] =
--       "build-info" J..= J.Null
--     | otherwise =
--       "build-info" J..= J.String (buildInfoPref dist_dir)

--   dist_dir :: FilePath
--   dist_dir =
--     distBuildDirectory
--       distDirLayout
--       (elabDistDirParams elaboratedSharedConfig elab)

--   bin_file :: ComponentDeps.Component -> [J.Pair]
--   bin_file c = case c of
--     ComponentDeps.ComponentExe s -> bin_file' s
--     ComponentDeps.ComponentTest s -> bin_file' s
--     ComponentDeps.ComponentBench s -> bin_file' s
--     ComponentDeps.ComponentFLib s -> flib_file' s
--     _ -> []
--   bin_file' s =
--     ["bin-file" J..= J.String bin]
--     where
--       bin =
--         if elabBuildStyle elab == BuildInplaceOnly
--           then dist_dir </> "build" </> prettyShow s </> prettyShow s <.> exeExtension plat
--           else InstallDirs.bindir (elabInstallDirs elab) </> prettyShow s <.> exeExtension plat

--   flib_file' :: (Pretty a, Show a) => a -> [J.Pair]
--   flib_file' s =
--     ["bin-file" J..= J.String bin]
--     where
--       bin =
--         if elabBuildStyle elab == BuildInplaceOnly
--           then dist_dir </> "build" </> prettyShow s </> ("lib" ++ prettyShow s) <.> dllExtension plat
--           else InstallDirs.bindir (elabInstallDirs elab) </> ("lib" ++ prettyShow s) <.> dllExtension plat

repoToNix :: Repo -> J.Value
repoToNix repo =
  case repo of
    RepoLocalNoIndex {..} ->
      J.object
        [ "type" J..= J.String "local-repo-no-index",
          "path" J..= J.String repoLocalDir
        ]
    RepoRemote {..} ->
      J.object
        [ "type" J..= J.String "remote-repo",
          "uri" J..= J.String (show (remoteRepoURI repoRemote))
        ]
    RepoSecure {..} ->
      J.object
        [ "type" J..= J.String "secure-repo",
          "uri" J..= J.String (show (remoteRepoURI repoRemote))
        ]

comp2str :: ComponentDeps.Component -> String
comp2str = prettyShow

style2str :: Bool -> BuildStyle -> String
style2str True _ = "local"
style2str False BuildInplaceOnly = "inplace"
style2str False BuildAndInstall = "global"

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""
