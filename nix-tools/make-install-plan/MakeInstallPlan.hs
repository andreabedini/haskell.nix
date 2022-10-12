{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String (IsString)
import qualified Data.Text as T
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( CabalDirLayout,
    defaultDistDirLayout,
    mkCabalDirLayout,
  )
import Distribution.Client.GlobalFlags
import Distribution.Client.HashValue (HashValue, showHashValue)
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
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo
import qualified Distribution.Client.Utils.Json as J
import Distribution.Compat.Directory (makeAbsolute)
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Types.PackageId
import Distribution.Types.SourceRepo
import Distribution.Verbosity (moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import Network.URI (URI (uriPath))
import Nix
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (hPutDoc)
import System.Environment (getArgs)
import System.FilePath
import System.IO

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
  let ProjectConfigShared
        { projectConfigDistDir,
          projectConfigProjectFile
        } = projectConfigShared cliConfig
  let mProjectFile = flagToMaybe projectConfigProjectFile
  let mDistDirectory = flagToMaybe projectConfigDistDir

  Right projectRoot <- findProjectRoot Nothing mProjectFile

  let distDirLayout = defaultDistDirLayout projectRoot mDistDirectory

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
  (_improvedPlan, elaboratedPlan, _elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  writeDoc "plan.nix" $ prettyNix $ makeNixPlan elaboratedPlan

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

makeNixPlan :: ElaboratedInstallPlan -> Fix (Compose IO NExprF)
makeNixPlan elaboratedInstallPlan =
  "makeSdistFromGitSourceRepo" ==> "self"
    ==> mkNonRecSet [elaboratedPackageToNix ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedInstallPlan]
  where
    elaboratedPackageToNix :: ElaboratedConfiguredPackage -> Binding NExpr
    elaboratedPackageToNix
      ElaboratedConfiguredPackage
        { elabUnitId,
          elabPkgSourceId = PackageIdentifier {pkgName, pkgVersion},
          elabFlagAssignment,
          elabPkgSourceLocation,
          elabPkgSourceHash
        } =
        T.pack (quoted $ prettyShow elabUnitId)
          $= mkNonRecSet
            [ "pkg-name" $= mkStr ((T.pack . prettyShow) pkgName),
              "pkg-version" $= mkStr ((T.pack . prettyShow) pkgVersion),
              "flags" $= flagAssignmentToNix elabFlagAssignment,
              "pkg-src" $= packageLocationToNix elabPkgSourceLocation elabPkgSourceHash
            ]

packageLocationToNix :: PackageLocation (Maybe FilePath) -> Maybe PackageSourceHash -> NExpr
packageLocationToNix (RepoTarballPackage (RepoLocalNoIndex _lr _s) _pkgId _local) (Just _srcHash) =
  error "not implemented"
packageLocationToNix (RepoTarballPackage RepoRemote {repoRemote} pkgId _local) (Just srcHash) =
  fetchRemoteRepoPackage repoRemote pkgId srcHash
packageLocationToNix (RepoTarballPackage RepoSecure {repoRemote} pkgId _local) (Just srcHash) =
  fetchRemoteRepoPackage repoRemote pkgId srcHash
packageLocationToNix (LocalUnpackedPackage local) _ =
  mkPath False local
packageLocationToNix (LocalTarballPackage local) _ =
  mkPath False local
packageLocationToNix (RemoteTarballPackage uri _local) (Just srcHash) =
  fetch uri srcHash
packageLocationToNix (RemoteSourceRepoPackage srcRepo _) (Just srcHash) =
  case srcRepo of
    SourceRepositoryPackage (KnownRepoType Git) srpLocation srpTag srpBranch srpSubdir _ ->
      "makeSdistFromGitSourceRepo"
        @@ mkNonRecSet
          [ "location" $= mkStr (T.pack srpLocation),
            "branch" $= maybe mkNull (mkStr . T.pack) srpBranch,
            "tag" $= maybe mkNull (mkStr . T.pack) srpTag,
            "subdir" $= maybe mkNull (mkStr . T.pack) srpSubdir,
            "sdist-sha256" $= mkStr (T.pack $ showHashValue srcHash)
          ]
    SourceRepositoryPackage {srpType} ->
      error $ show srpType ++ " not supported"
packageLocationToNix RemoteSourceRepoPackage {} Nothing = error "No hash available"
packageLocationToNix RemoteTarballPackage {} Nothing = error "No hash available"
packageLocationToNix RepoTarballPackage {} Nothing = error "No hash available"

fetchRemoteRepoPackage :: RemoteRepo -> PackageId -> PackageSourceHash -> NExpr
fetchRemoteRepoPackage RemoteRepo {remoteRepoURI} pkgId =
  fetch uri
  where
    uri =
      remoteRepoURI
        { uriPath = uriPath remoteRepoURI </> "package" </> prettyShow pkgId </> prettyShow pkgId <.> ".tar.gz"
        }

fetch :: URI -> HashValue -> NExpr
fetch uri hash =
  "builtins" @. "fetchurl"
    @@ mkNonRecSet
      [ "url" $= mkStr ((T.pack . show) uri),
        "sha256" $= mkStr (T.pack $ showHashValue hash)
      ]

flagAssignmentToNix :: PD.FlagAssignment -> NExpr
flagAssignmentToNix flagAssignment =
  mkNonRecSet [(T.pack . prettyShow) fn $= mkBool fv | (fn, fv) <- PD.unFlagAssignment flagAssignment]

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  withFile file WriteMode $ \handle ->
    hPutDoc handle doc
