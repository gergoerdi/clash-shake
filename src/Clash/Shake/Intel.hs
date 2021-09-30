{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.Intel
    ( Target(..)
    , de0Nano, arrowDeca

    , quartus
    ) where

import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T

data Target = Target
    { targetFamily :: String
    , targetDevice :: String
    }

targetMustache Target{..} =
    [ "targetFamily"  .= T.pack targetFamily
    , "targetDevice"  .= T.pack targetDevice
    ]

de0Nano :: Target
de0Nano = Target "Cyclone IV E" "EP4CE22F17C6"

arrowDeca :: Target
arrowDeca = Target "MAX 10" "10M50DAF484C6GES"

quartus :: Target -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
quartus fpga kit@ClashKit{..} outDir srcDir topName = do
    let projectName = topName
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let quartus tool args = cmd_ (Cwd outDir) =<< toolchain "QUARTUS" tool args

    let getFiles dir pats = getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
        hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v", "*.sv"]
        tclSrcs = getFiles "src-hdl" ["*.tcl"]
        constrSrcs = getFiles "src-hdl" ["*.sdc"]
        ipCores = getFiles "ip" ["//*.qip"]

    outDir <//> "*.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        srcs2 <- hdlSrcs
        tcls <- tclSrcs
        constrs <- constrSrcs
        cores <- ipCores

        let template = $(TH.compileMustacheFile "template/intel-quartus/project.tcl.mustache")
        let values = object . mconcat $
                [ [ "project" .= T.pack projectName ]
                  , [ "top" .= T.pack topName ]
                  , targetMustache fpga
                  , [ "srcs" .= mconcat
                             [ [ object [ "fileName" .= (rootDir </> src) ] | src <- srcs1 ]
                               , [ object [ "fileName" .= (rootDir </> srcDir </> src) ] | src <- srcs2 ]
                             ]
                    ]
                  , [ "tclSrcs" .= [ object [ "fileName" .= (rootDir </> srcDir </> src) ] | src <- tcls ] ]
                  , [ "ipcores" .= [ object [ "fileName" .= (rootDir </> srcDir </> core) ] | core <- cores ] ]
                  , [ "constraintSrcs" .= [ object [ "fileName" .= (rootDir </> srcDir </> src) ] | src <- constrs ] ]
                ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    outDir </> "ip" <//> "*" %> \out -> do
            let src = srcDir </> makeRelative outDir out
            copyFileChanged src out

    outDir </> topName <.> "sof" %> \_out -> do
        srcs1 <- manifestSrcs
        srcs2 <- hdlSrcs
        cores <- ipCores
        need $ mconcat
            [ [ outDir </> projectName <.> "tcl" ]
              , [ src | src <- srcs1 ]
              , [ srcDir </> src | src <- srcs2 ]
              , [ outDir </> core | core <- cores ]
            ]
        quartus "quartus_sh" ["-t", projectName <.> "tcl"]

    return $ SynthKit
        { bitfile = outDir </> topName <.> "sof"
          , phonies =
              [ "quartus" |> do
                  need [outDir </> projectName <.> "tcl"]
                  quartus "quartus_sh" ["-t", outDir </> projectName <.> "tcl"]
              ]
        }
