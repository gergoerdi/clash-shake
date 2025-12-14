{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Shake.Gowin (
    -- * Toolchain
    Target (..),
    Board (..),
    apycula,

    -- * Boards
    tangNano9k,
) where

import Clash.Shake
import Clash.Shake.F4PGA (openFPGALoader)
import Development.Shake
import Development.Shake.FilePath
import Text.Printf (printf)

data Target = Target
    { targetFamily :: String
    , targetDevice :: String
    }

data Board = Board
    { boardSpec :: String
    , boardTarget :: Target
    }

tangNano9k :: Board
tangNano9k = Board "tangnano9k" $ Target "GW1N-9C" "GW1NR-LV9QN88PC6/I5"

apycula :: Board -> SynthRules
apycula Board{boardSpec, boardTarget = Target{..}} kit@ClashKit{..} outDir topName extraGenerated = do
    let yosys :: String -> [String] -> Action ()
        yosys tool args = cmd_ (EchoStdout False) =<< toolchain "YOSYS" tool args
    let json = outDir </> topName <.> "json"
    let pnrjson = outDir </> ("pnr" <> topName) <.> "json"
    let bitfile = outDir </> topName <.> "fs"

    outDir </> topName <.> "ys" %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs
        writeFileChanged out $
            unlines
                [ printf "read_verilog %s" $ unwords (srcs <> verilogs)
                , printf "synth_gowin -top %s -json %s" topName json
                ]

    json %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs

        let ys = out -<.> "ys"
        need [ys]
        yosys "yosys" ["-q", ys]

    pnrjson %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        let [cst] = extraFiles ["//*.cst"]

        need [cst, json]
        yosys "nextpnr-himbaechel" ["--json", json, "--write", pnrjson, "--device", targetDevice, "--vopt", "family=" <> targetFamily, "--vopt", "cst=" <> cst]

    bitfile %> \out -> do
        need [pnrjson]
        cmd_ (EchoStdout True) =<< toolchain "APYCULA" "gowin_pack" ["-d", targetFamily, "-o", bitfile, pnrjson]

    pure
        SynthKit
            { bitfile = bitfile
            , phonies = ["upload" |> openFPGALoader ["-b", boardSpec] bitfile]
            }
