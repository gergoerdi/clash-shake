{-# LANGUAGE RecordWildCards #-}
module Clash.Shake
    ( HDL(..)
    , nestedPhony
    , (|>)

    , useConfig
    , RunClash(..), ClashKit(..)
    , clashRules
    , SynthKit(..)
    , findFiles
    , staticFiles
    , SynthRules

    , binImage

    , toolchain

    , withTargets
    ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util (parseMakefile)

import qualified Clash.Main as Clash

import Data.List.Split
import Text.Printf

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (isUpper, toLower)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import Control.Exception (bracket)
import Data.Maybe (fromJust)
import Data.Bits

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

hdlClashFlag :: HDL -> String
hdlClashFlag VHDL = "--vhdl"
hdlClashFlag Verilog = "--verilog"
hdlClashFlag SystemVerilog = "--systemverilog"

type RunClash = [String] -> Action ()

data ClashKit = ClashKit
    { manifestSrcs :: Action [FilePath]
    }

instance Semigroup ClashKit where
    kit <> kit' = ClashKit
        { manifestSrcs = (<>) <$> manifestSrcs kit <*> manifestSrcs kit'
        }

instance Monoid ClashKit where
    mempty = ClashKit
        { manifestSrcs = pure mempty
        }

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir act =
    bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \_ ->
        Dir.setCurrentDirectory dir >> act

clashRules :: FilePath -> HDL -> [FilePath] -> FilePath -> [String] -> Action () -> Rules (RunClash, ClashKit)
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

    outDir </> "ghc-deps.make" %> \out -> do
        alwaysRerun
        -- By writing to a temp file and using `copyFileChanged`,
        -- we avoid spurious reruns
        -- (https://stackoverflow.com/a/64277431/477476)
        withTempFileWithin outDir $ \tmp -> do
            clash ["-M", "-dep-suffix", "", "-dep-makefile", tmp, src]
            liftIO $ removeFiles outDir [takeBaseName tmp <.> "bak"]
            copyFileChanged tmp out

    manifestFile %> \_out -> do
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
        need hsDeps
        extraGenerated
        clash [hdlClashFlag hdl, src]

    return (clash, ClashKit{..})

data SynthKit = SynthKit
    { bitfile :: FilePath
    , phonies :: [(String, Action ())]
    }

type SynthRules = ClashKit -> FilePath -> String -> Action [FilePath] -> Rules SynthKit

findFiles :: [FilePath] -> [FilePattern] -> [FilePath]
findFiles universe pats = filter (\fn -> (?== fn) `any` pats) universe

nestedPhony :: String -> String -> Action () -> Rules ()
nestedPhony target name = phony (target <> ":" <> name)

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
      want [target <> ":" <> "bitfile"]

binImage :: Maybe Int -> FilePath -> FilePath -> Action ()
binImage size src out = do
    need [src]
    lines <- liftIO $ binLines size <$> BS.readFile src
    writeFileChanged out (unlines lines)

binLines :: Maybe Int -> BS.ByteString -> [String]
binLines size bs = map bitsOf bytes
  where
    bytes = maybe id ensureSize size $ BS.unpack bs
    ensureSize size bs = take size $ bs <> repeat 0x00

bitsOf :: (FiniteBits a) => a -> [Char]
bitsOf x = reverse $ go (finiteBitSize x) x
  where
    go 0 _ = []
    go n x = (if testBit x 0 then '1' else '0') : go (n-1) (x `shiftR` 1)

isModuleName :: String -> Bool
isModuleName = all (isUpper . head) . splitOn "."

toolchain :: String -> FilePath -> [String] -> Action [String]
toolchain name tool args = do
    wrap <- getConfig name
    root <- getConfig $ name <> "_ROOT"
    let exe = case (wrap, root) of
            (Just wrap, _) -> [wrap, tool]
            (Nothing, Just root) -> [root </> tool]
            (Nothing, Nothing) -> [tool]
    return $ exe ++ args

(|>) :: String -> Action () -> (String, Action ())
(|>) = (,)

withTargets :: [String] -> Rules a -> Rules a
withTargets targets rules
  | null targets = rules
  | otherwise = want targets >> withoutActions rules

staticFiles :: FilePath -> Action [FilePath]
staticFiles dir = do
    files <- map (dir </>) <$> getDirectoryFiles dir ["//*"]
    need files
    return files
