{-# LANGUAGE LambdaCase #-}

module Plan2Nix.Project
  ( findCabalFiles,
  )
where

import Cabal2Nix (CabalFile (..), CabalFileGenerator (..))
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

findCabalFiles :: FilePath -> IO [CabalFile]
findCabalFiles path =
  doesFileExist (path </> Hpack.packageConfig) >>= \case
    False -> fmap (OnDisk . (path </>)) . filter (isSuffixOf ".cabal") <$> listDirectory path
    True -> do
      mbPkg <- Hpack.readPackageConfig Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = path </> Hpack.packageConfig}
      case mbPkg of
        Left e -> error e
        Right r ->
          return [InMemory (Just Hpack) (Hpack.decodeResultCabalFile r) (encodeUtf8 $ render r)]
  where
    render :: Hpack.DecodeResult -> String
    render r =
      let body = Hpack.renderPackage [] (Hpack.decodeResultPackage r)
          cabalVersion = Hpack.decodeResultCabalVersion r
       in cabalVersion ++ body

    encodeUtf8 :: String -> ByteString
    encodeUtf8 = T.encodeUtf8 . T.pack
