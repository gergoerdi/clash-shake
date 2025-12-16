{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.ECP5
    ( ecp5
    , fujprog
    ) where

import Clash.Shake
import Clash.Shake.F4PGA

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config

ecp5 :: String -> SynthRules
ecp5 device kit@ClashKit{..} outDir topName extraGenerated = do
    json <- yosysRules kit outDir topName extraGenerated "ecp5"

    outDir </> topName <.> "config" %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        let [lpf] = extraFiles ["//*.lpf"]
        let json = out -<.> "json"

        need [lpf, json]
        yosys "nextpnr-ecp5" ["--json", json, "--textcfg", out, "--" <> device, "--lpf", lpf]

    outDir </> topName <.> "bit" %> \out -> do
        let config = out -<.> "config"
        need [config]

        yosys "ecppack" ["--input", config, "--bit", out]

    let bitfile = outDir </> topName <.> "bit"

    pure SynthKit
        { bitfile = bitfile
        , phonies =
            [ "upload" |> fujprog [] bitfile
            ]
        }

fujprog :: [String] -> FilePath -> Action ()
fujprog args bitfile = do
    need [bitfile]
    cmd_ =<< toolchain "FUJPROG" "fujprog" (args ++ [bitfile])
