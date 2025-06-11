{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.Xilinx
    ( Target(..), targetPart
    , Board(..)
    , ise
    , vivado

    , papilioPro, papilioOne, nexysA750T, basys3, pynqZ2
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

-- | Target defintion for Papilio Pro
papilioPro :: Target
papilioPro = Target "Spartan6" "xc6slx9" "tqg144" 2

-- | Target definition for Papilio One
papilioOne :: Target
papilioOne = Target "Spartan3E" "xc3s500e" "vq100" 5

data Board = Board
    { boardSpec :: String -- TODO: what is the structure of this?
    , boardDeviceIndex :: Word
    , boardTarget :: Target
    }

boardDeviceName :: Board -> String
boardDeviceName Board{ boardTarget = Target{..}, .. } = targetDevice <> "_" <> show boardDeviceIndex

boardMustache :: Board -> [Aeson.Pair]
boardMustache board@Board{..} =
    [ "board"      .= T.pack boardSpec
    , "deviceName" .= T.pack (boardDeviceName board)
    ] <>
    targetMustache boardTarget

-- | Board definition for Digilent Nexys A7-50T
nexysA750T :: Board
nexysA750T = Board "digilentinc.com:nexys-a7-50t:part0:1.0" 0 $
    Target "artix7" "xc7a50t" "csg324" 1

-- | Board definition for Digilent Basys 3
basys3 :: Board
basys3 = Board "digilentinc.com:basys3:part0:1.2" 0 $
    Target "artix7" "xc7a35t" "cpg236" 1

-- | Board definition for TUL PYNQ-Z2
pynqZ2 :: Board
pynqZ2 = Board "tul.com.tw:pynq-z2:part0:1.0" 1 $
    Target "zynq7000" "xc7z020" "clg400" 1

ise :: Target -> SynthRules
ise fpga kit@ClashKit{..} outDir topName extraGenerated = do
    let projectName = topName
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let ise tool args = cmd_ (Cwd outDir) =<< toolchain "ISE" tool args

    outDir </> "project.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        extraFiles <- findFiles <$> extraGenerated
        let srcs2 = extraFiles ["//*.vhdl", "//*.v", "//*.ucf"]
            cores = extraFiles ["//*.xco", "//*.xaw"]

        let template = $(TH.compileMustacheFile "template/xilinx-ise/project.tcl.mustache")
        let values = object . mconcat $
                     [ [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , targetMustache fpga
                     , [ "srcs" .= mconcat
                         [ [ object [ "fileName" .= (rootDir </> src) ] | src <- srcs1 <> srcs2 ]
                         , [ object [ "fileName" .= ("ipcore_dir" </> takeFileName core) ] | core <- cores ]
                         ]
                       ]
                     , [ "ipcores" .= [ object [ "name" .= takeBaseName core ] | core <- cores ] ]
                     ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    outDir </> "ipcore_dir" <//> "*" %> \out -> do
        let src = makeRelative (outDir </> "ipcore_dir") out
        extraFiles <- findFiles <$> extraGenerated
        case extraFiles ["//" <> src] of
          [oneFile] -> copyFileChanged oneFile out
          [] -> error $ unwords ["Cannot find IP core file", src]
          multiple ->  error $ unwords ["Multiple candidates for IP core file", src, show multiple]

    outDir </> topName <.> "bit" %> \_out -> do
        extraFiles <- findFiles <$> extraGenerated
        let cores = extraFiles ["//*.xco", "//*.xaw"]
        need $ mconcat
          [ [ outDir </> "project.tcl" ]
          , [ outDir </> "ipcore_dir" </> takeFileName core | core <- cores ]
          ]
        ise "xtclsh" ["project.tcl", "rebuild_project"]

    return $ SynthKit
        { bitfile = outDir </> topName <.> "bit"
        , phonies =
            [ "ise" |> do
                   need ["project.tcl"]
                   ise "ise" ["project.tcl"]
            ]
        }

vivado :: Board -> SynthRules
vivado board kit@ClashKit{..} outDir topName extraGenerated = do
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

    xpr %> \out -> vivadoBatch "project.tcl"

    outDir </> "project.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        extraFiles <- findFiles <$> extraGenerated
        let srcs2 = extraFiles ["//*.vhdl", "//*.v" ]
            cores = extraFiles ["//*.xci"]
            constrs = extraFiles ["//*.xdc"]

        let template = $(TH.compileMustacheFile "template/xilinx-vivado/project.tcl.mustache")
        let values = object . mconcat $
                     [ [ "rootDir" .= T.pack rootDir]
                     , [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , boardMustache board
                     , [ "srcs" .= mconcat
                         [ [ object [ "fileName" .= src ] | src <- srcs1 <> srcs2 ]
                         ]
                       ]
                     , [ "coreSrcs" .= object
                         [ "nonempty" .= not (null cores)
                         , "items" .= [ object [ "fileName" .= core ] | core <- cores ]
                         ]
                       ]
                     , [ "ipcores" .= [ object [ "name" .= takeBaseName core ] | core <- cores ] ]
                     , [ "constraintSrcs" .= [ object [ "fileName" .= src ] | src <- constrs ] ]
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
                   vivado "vivado" [makeRelative outDir xpr]
            , "upload" |> do
                   need [projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit"]
                   vivadoBatch "upload.tcl"
            ]
        }
