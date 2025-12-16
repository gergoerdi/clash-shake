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
import Clash.Shake.F4PGA

import Development.Shake
import Development.Shake.FilePath

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
    let pnrjson = outDir </> ("pnr" <> topName) <.> "json"
    let bitfile = outDir </> topName <.> "fs"

    json <- yosysRules kit outDir topName extraGenerated "gowin"

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
