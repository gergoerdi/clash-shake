{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Clash.Shake
    ( HDL(..)
    , nestedPhony
    , ClashKit(..)
    , clashRules
    , SynthKit(..)
    , XilinxTarget(..), papilioPro, papilioOne, nexysA750T
    , xilinxISE
    , xilinxVivado
    , binImage
    , useConfig
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import qualified Clash.Main as Clash

import Control.Monad.Trans

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson
import Data.String (fromString)
import Data.Char (isUpper)
import Data.Binary
import Control.DeepSeq
import Data.Hashable
import Data.List.Split

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.List (sort, nub)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Control.Monad (guard, msum)
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import Control.Exception

import Clash.Driver.Types
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

data XilinxTarget = XilinxTarget
    { targetFamily :: String
    , targetDevice :: String
    , targetPackage :: String
    , targetSpeed :: String
    }

targetMustache XilinxTarget{..} =
    [ "targetFamily"  .= T.pack targetFamily
    , "targetDevice"  .= T.pack targetDevice
    , "targetPackage" .= T.pack targetPackage
    , "targetSpeed"   .= T.pack targetSpeed
    , "part"          .= T.pack (targetDevice <> targetPackage <> targetSpeed)
    ]

papilioPro :: XilinxTarget
papilioPro = XilinxTarget "Spartan6" "xc6slx9" "tqg144" "-2"

papilioOne :: XilinxTarget
papilioOne = XilinxTarget "Spartan3E" "xc3s500e" "vq100" "-5"

nexysA750T :: XilinxTarget
nexysA750T = XilinxTarget "Artrix7" "xc7a50t" "icsg324" "-1L"

data ClashKit = ClashKit
    { clash :: [String] -> Action ()
    , buildDir :: FilePath
    , unBuildDir :: FilePath -> FilePath
    , manifestSrcs :: Action [FilePath]
    }

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir act =
    bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \_ ->
        Dir.setCurrentDirectory dir >> act

clashRules :: FilePath -> FilePath -> HDL -> [FilePath] -> FilePath -> [String] -> Action () -> Rules ClashKit
clashRules buildDir targetDir hdl srcDirs src clashFlags extraGenerated = do
    let synDir = buildDir </> targetDir
        upBuildDir = foldr (</>) "." $ replicate (length $ splitPath buildDir) ".."
        unBuildDir dir = upBuildDir </> dir
        inBuildDir = withWorkingDirectory buildDir

    let clash args = liftIO $ do
            let srcFlags = ["-i" <> unBuildDir srcDir | srcDir <- srcDirs]
            let args' = ["-outputdir", targetDir] <> clashFlags <> srcFlags <> args
            putStrLn $ "Clash.defaultMain " <> unwords args'
            inBuildDir $ Clash.defaultMain args'

    -- TODO: ideally, Clash should return the manifest, or at least its file location...
    let (synModule, srcArg)
          | isModuleName src = (last . splitOn "." $ src, src)
          | otherwise = ("Main", unBuildDir src)

        clashTopName = "topEntity"
        synOut = synDir </> hdlDir hdl </> synModule </> clashTopName
        manifest = do
            let manifestFile = synOut </> clashTopName <.> "manifest"
            need [manifestFile]
            read <$> readFile' manifestFile

    let manifestSrcs = do
            Manifest{..} <- manifest
            let clashSrcs = map T.unpack componentNames <>
                            [ map toLower clashTopName <> "_types" | hdl == VHDL ]
            return [ synOut </> c <.> hdlExt hdl | c <- clashSrcs ]

    getSrcs <- do
        synDir </> "ghc-deps.make" %> \out -> do
            alwaysRerun
            -- By writing to a temp file and using `copyFileChanged`,
            -- we avoid spurious reruns
            -- (https://stackoverflow.com/a/64277431/477476)
            let tmp = out <.> "tmp"
            clash ["-M", "-dep-suffix", "", "-dep-makefile", unBuildDir tmp, srcArg]
            copyFileChanged tmp out
            liftIO $ removeFiles "." [tmp, tmp <.> "bak"]

        return $ do
            let depFile = synDir </> "ghc-deps.make"
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

    synDir </> hdlDir hdl <//> "*.manifest" %> \out -> do
        srcs <- getSrcs
        need [buildDir </> src | src <- srcs]
        extraGenerated
        clash [case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }, srcArg]

    return ClashKit{..}

data SynthKit = SynthKit
    { bitfile :: FilePath
    , phonies :: [(String, Action ())]
    }

nestedPhony :: String -> String -> Action () -> Rules ()
nestedPhony root name = phony (root </> name)

(|>) :: String -> Action () -> (String, Action ())
(|>) = (,)

xilinxISE :: XilinxTarget -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
xilinxISE fpga kit@ClashKit{..} targetDir srcDir topName = do
    let projectName = topName
        outDir = buildDir </> targetDir
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let ise tool args = do
            root <- getConfig "ISE_ROOT"
            wrap <- getConfig "ISE"
            let exe = case (wrap, root) of
                    (Just wrap, _) -> [wrap, tool]
                    (Nothing, Just root) -> [root </> "ISE/bin/lin64" </> tool]
                    (Nothing, Nothing) -> error "ISE_ROOT or ISE must be set in build.mk"
            cmd_ (Cwd outDir) exe args

    let getFiles dir pats = getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
        hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v", "*.ucf" ]
        ipCores = getFiles "ipcore_dir" ["*.xco", "*.xaw"]

    outDir <//> "*.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        srcs2 <- hdlSrcs
        cores <- ipCores

        let template = $(TH.compileMustacheFile "template/xilinx-ise/project.tcl.mustache")
        let values = object . mconcat $
                     [ [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , targetMustache fpga
                     , [ "srcs" .= mconcat
                         [ [ object [ "fileName" .= (rootDir </> src) ] | src <- srcs1 ]
                         , [ object [ "fileName" .= (rootDir </> srcDir </> src) ] | src <- srcs2 ]
                         , [ object [ "fileName" .= core ] | core <- cores ]
                         ]
                       ]
                     , [ "ipcores" .= [ object [ "name" .= takeBaseName core ] | core <- cores ] ]
                     ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    outDir </> "ipcore_dir" <//> "*" %> \out -> do
        let src = srcDir </> makeRelative outDir out
        copyFileChanged src out

    outDir </> topName <.> "bit" %> \_out -> do
        srcs1 <- manifestSrcs
        srcs2 <- hdlSrcs
        cores <- ipCores
        need $ mconcat
            [ [ outDir </> projectName <.> "tcl" ]
            , [ src | src <- srcs1 ]
            , [ srcDir </> src | src <- srcs2 ]
            , [ outDir </> core | core <- cores ]
            ]
        ise "xtclsh" [projectName <.> "tcl", "rebuild_project"]

    return $ SynthKit
        { bitfile = outDir </> topName <.> "bit"
        , phonies =
            [ "ise" |> do
                   need [outDir </> projectName <.> "tcl"]
                   ise "ise" [outDir </> projectName <.> "tcl"]
            ]
        }

xilinxVivado :: XilinxTarget -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
xilinxVivado fpga kit@ClashKit{..} targetDir srcDir topName = do
    let projectName = topName
        outDir = buildDir </> targetDir
        projectDir = outDir </> projectName
        xpr = projectDir </> projectName <.> "xpr"
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let vivado tool args = do
            root <- getConfig "VIVADO_ROOT"
            wrap <- getConfig "VIVADO"
            let exe = case (wrap, root) of
                    (Just wrap, _) -> [wrap, tool]
                    (Nothing, Just root) -> [root </> "bin" </> tool]
                    (Nothing, Nothing) -> error "VIVADO_ROOT or VIVADO must be set in build.mk"
            cmd_ (Cwd outDir) exe args
        vivadoBatch tcl = do
            need [outDir </> tcl]
            vivado "vivado"
              [ "-mode", "batch"
              , "-nojournal"
              , "-nolog"
              , "-source", tcl
              ]

    let getFiles dir pats = getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
        hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v" ]
        constrSrcs = getFiles "src-hdl" ["*.xdc" ]
        ipCores = getFiles "ip" ["*.xci"]

    xpr %> \out -> vivadoBatch "project.tcl"

    outDir </> "project.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        srcs2 <- hdlSrcs
        cores <- ipCores
        constrs <- constrSrcs

        let template = $(TH.compileMustacheFile "template/xilinx-vivado/project.tcl.mustache")
        let values = object . mconcat $
                     [ [ "rootDir" .= T.pack rootDir]
                     , [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , targetMustache fpga
                     , [ "board" .= T.pack "digilentinc.com:nexys-a7-50t:part0:1.0" ] -- TODO
                     , [ "srcs" .= mconcat
                         [ [ object [ "fileName" .= src ] | src <- srcs1 ]
                         , [ object [ "fileName" .= (srcDir </> src) ] | src <- srcs2 ]
                         ]
                       ]
                     , [ "coreSrcs" .= object
                         [ "nonempty" .= not (null cores)
                         , "items" .= [ object [ "fileName" .= (srcDir </> core) ] | core <- cores ]
                         ]
                       ]
                     , [ "ipcores" .= [ object [ "name" .= takeBaseName core ] | core <- cores ] ]
                     , [ "constraintSrcs" .= [ object [ "fileName" .= (srcDir </> src) ] | src <- constrs ] ]
                     ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    outDir </> "build.tcl" %> \out -> do
        let template = $(TH.compileMustacheFile "template/xilinx-vivado/project-build.tcl.mustache")
        let values = object . mconcat $
                     [ [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    outDir </> "upload.tcl" %> \out -> do
        let template = $(TH.compileMustacheFile "template/xilinx-vivado/upload.tcl.mustache")
        let values = object . mconcat $
                     [ [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , targetMustache fpga
                     ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit" %> \out -> do
        need [xpr]
        vivadoBatch "build.tcl"

    return SynthKit
        { bitfile = projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit"
        , phonies =
            [ "vivado" |> do
                   need [xpr]
                   vivado "vivado" [xpr]
            , "upload" |> do
                   need [projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit"]
                   vivadoBatch "upload.tcl"
            ]
        }

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
