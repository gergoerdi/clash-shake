{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.Xilinx
    ( Target(..), targetPart
    , Board(..)
    , ise
    , vivado

    , papilioPro, papilioOne, nexysA750T, basys3
    ) where

import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T

data Target = Target
    { targetFamily :: String
    , targetDevice :: String
    , targetPackage :: String
    , targetSpeed :: Word
    }

targetPart :: Target -> String
targetPart Target{..} = targetDevice <> targetPackage <> "-" <> show targetSpeed

targetMustache :: Target -> [Aeson.Pair]
targetMustache target@Target{..} =
    [ "targetFamily"  .= T.pack targetFamily
    , "targetDevice"  .= T.pack targetDevice
    , "targetPackage" .= T.pack targetPackage
    , "targetSpeed"   .= targetSpeed
    , "part"          .= T.pack (targetPart target)
    ]

papilioPro :: Target
papilioPro = Target "Spartan6" "xc6slx9" "tqg144" 2

papilioOne :: Target
papilioOne = Target "Spartan3E" "xc3s500e" "vq100" 5

data Board = Board
    { boardSpec :: String -- TODO: what is the structure of this?
    , boardTarget :: Target
    }

boardMustache :: Board -> [Aeson.Pair]
boardMustache Board{..} =
    [ "board" .= T.pack boardSpec
    ] <>
    targetMustache boardTarget

nexysA750T :: Board
nexysA750T = Board "digilentinc.com:nexys:a7-50t:part0:1.0" $
    Target "artix7" "xc7a50t" "csg324" 1

basys3 :: Board
basys3 = Board "digilentinc.com:basys3:part0:1.2" $
    Target "artix7" "xc7a35t" "cpg236" 1


ise :: Target -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
ise fpga kit@ClashKit{..} outDir srcDir topName = do
    let projectName = topName
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let ise tool args = cmd_ (Cwd outDir) =<< toolchain "ISE" tool args

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

vivado :: Board -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
vivado board kit@ClashKit{..} outDir srcDir topName = do
    let projectName = topName
        projectDir = outDir </> projectName
        xpr = projectDir </> projectName <.> "xpr"
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let vivado tool args = cmd_ (Cwd outDir) =<< toolchain "VIVADO" tool args
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
                     , boardMustache board
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
                     , boardMustache board
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
