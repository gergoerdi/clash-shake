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

-- | Target definition for Terasic DE0-Nano
de0Nano :: Target
de0Nano = Target "Cyclone IV E" "EP4CE22F17C6"

-- | Target definition for Arrow DECA
arrowDeca :: Target
arrowDeca = Target "MAX 10" "10M50DAF484C6GES"

quartus :: Target -> SynthRules
quartus fpga kit@ClashKit{..} outDir topName extraGenerated = do
    let projectName = topName
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let quartus tool args = cmd_ (Cwd outDir) =<< toolchain "QUARTUS" tool args

    outDir <//> "*.tcl" %> \out -> do
        srcs1 <- manifestSrcs
        extraFiles <- findFiles <$> extraGenerated
        let srcs2 = extraFiles ["//*.vhdl", "//*.v", "//*.sv"]
            tcls = extraFiles ["//*.tcl"]
            constrs = extraFiles ["//*.sdc"]
            cores = extraFiles ["//*.qip"]

        let template = $(TH.compileMustacheFile "template/intel-quartus/project.tcl.mustache")
        let values = object . mconcat $
                [ [ "project" .= T.pack projectName ]
                  , [ "top" .= T.pack topName ]
                  , targetMustache fpga
                  , [ "srcs" .= [ object [ "fileName" .= (rootDir </> src) ] | src <- srcs1 <> srcs2 ]
                    ]
                  , [ "tclSrcs" .= [ object [ "fileName" .= (rootDir </> src) ] | src <- tcls ] ]
                  , [ "ipcores" .= [ object [ "fileName" .= (rootDir </> core) ] | core <- cores ] ]
                  , [ "constraintSrcs" .= [ object [ "fileName" .= (rootDir </> src) ] | src <- constrs ] ]
                ]
        writeFileChanged out . TL.unpack $ renderMustache template values

    let bitfile = outDir </> topName <.> "sof"

    bitfile %> \_out -> do
        need $ [ outDir </> projectName <.> "tcl" ]
        quartus "quartus_sh" ["-t", projectName <.> "tcl"]

    outDir </> topName <.> "rbf" %> \out -> do
        let sof = out -<.> "sof"
        need [sof]
        quartus "quartus_cpf"
          [ "--option=bitstream_compression=off"
          , "-c", makeRelative outDir sof
          , makeRelative outDir out
          ]

    return $ SynthKit
        { bitfile = bitfile
          , phonies =
              [ "quartus" |> do
                    need [outDir </> projectName <.> "tcl"]
                    quartus "quartus_sh" ["-t", projectName <.> "tcl"]
              , "upload" |> do
                    need [bitfile]
                    quartus "quartus_pgm" ["-m", "jtag", "-o", "p;" <> makeRelative outDir bitfile]
              ]
        }
