{-# LANGUAGE RecordWildCards #-}
module Clash.Shake
    ( HDL(..)
    , nestedPhony
    , ClashKit(..)
    , clashRules
    , SynthKit(..)
    , binImage
    , useConfig
    ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util (parseMakefile)

import qualified Clash.Main as Clash

import Data.List.Split

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (isUpper, toLower)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import Control.Exception (bracket)
import Data.Maybe (fromJust)

import Clash.Driver.Manifest
import Clash.Prelude (pack)

data HDL
    = VHDL
    | Verilog
    | SystemVerilog
    deriving (Eq, Enum, Bounded, Show, Read)

hdlDir :: HDL -> FilePath
hdlDir VHDL = "vhdl"
hdlDir Verilog = "verilog"
hdlDir SystemVerilog = "systemverilog"

hdlExt :: HDL -> FilePath
hdlExt VHDL = "vhdl"
hdlExt Verilog = "v"
hdlExt SystemVerilog = "sv"

data ClashKit = ClashKit
    { clash :: [String] -> Action ()
    , manifestSrcs :: Action [FilePath]
    }

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir act =
    bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \_ ->
        Dir.setCurrentDirectory dir >> act

clashRules :: FilePath -> HDL -> [FilePath] -> FilePath -> [String] -> Action () -> Rules ClashKit
clashRules outDir hdl srcDirs src clashFlags extraGenerated = do
    let clash args = liftIO $ do
            let srcFlags = ["-i" <> srcDir | srcDir <- srcDirs]
            let args' = ["-outputdir", outDir] <> clashFlags <> srcFlags <> args
            putStrLn $ "Clash.defaultMain " <> unwords args'
            Clash.defaultMain args'

    -- TODO: ideally, Clash should return the manifest, or at least its file location...
    let synModule
          | isModuleName src = src
          | otherwise = "Main"

        clashTopName = "topEntity"
        synOut = outDir </> synModule <.> clashTopName
        manifestFile = synOut </> "clash-manifest.json"
        manifest = do
            need [manifestFile]
            Just manifest <- liftIO $ readManifest manifestFile
            return manifest

    let manifestSrcs = do
            Manifest{..} <- manifest
            let clashSrcs = map T.unpack componentNames <>
                            [ map toLower clashTopName <> "_types" | hdl == VHDL ]
            return [ synOut </> c <.> hdlExt hdl | c <- clashSrcs ]

    getSrcs <- do
        outDir </> "ghc-deps.make" %> \out -> do
            alwaysRerun
            -- By writing to a temp file and using `copyFileChanged`,
            -- we avoid spurious reruns
            -- (https://stackoverflow.com/a/64277431/477476)
            withTempFileWithin outDir $ \tmp -> do
                clash ["-M", "-dep-suffix", "", "-dep-makefile", tmp, src]
                liftIO $ removeFiles outDir [takeBaseName tmp <.> "bak"]
                copyFileChanged tmp out

        return $ do
            let depFile = outDir </> "ghc-deps.make"
            need [depFile]
            deps <- parseMakefile <$> liftIO (readFile depFile)
            let isHsSource fn
                  | ext `elem` [".hi"] = False
                  | ext `elem` [".hs", ".lhs"] = True
                  | otherwise = error $ "Unrecognized source file: " <> fn
                  where
                    ext = takeExtension fn
                hsDeps = [fn | (_, fns) <- deps, fn <- fns, isHsSource fn]
            return hsDeps

    manifestFile %> \_out -> do
        need =<< getSrcs
        extraGenerated
        clash [case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }, src]

    return ClashKit{..}

data SynthKit = SynthKit
    { bitfile :: FilePath
    , phonies :: [(String, Action ())]
    }

nestedPhony :: String -> String -> Action () -> Rules ()
nestedPhony root name = phony (root </> name)

useConfig :: FilePath -> Rules ()
useConfig file = do
    cfg <- do
        haveConfig <- liftIO $ Dir.doesFileExist file
        if haveConfig then do
            usingConfigFile file
            liftIO $ readConfigFile file
          else do
            usingConfig mempty
            return mempty

    forM_ (HM.lookup "TARGET" cfg) $ \target ->
      want [target </> "bitfile"]

binImage :: Maybe Int -> FilePath -> FilePath -> Action ()
binImage size src out = do
    need [src]
    lines <- liftIO $ binLines size <$> BS.readFile src
    writeFileChanged out (unlines lines)

binLines :: Maybe Int -> BS.ByteString -> [String]
binLines size bs = map (filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size $ BS.unpack bs
    ensureSize size bs = take size $ bs <> repeat 0x00

isModuleName :: String -> Bool
isModuleName = all (isUpper . head) . splitOn "."
