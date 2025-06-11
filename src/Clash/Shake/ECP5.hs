{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.ECP5
    ( ecp5
    , fujprog
    ) where

import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Text.Printf

ecp5 :: String -> SynthRules
ecp5 device kit@ClashKit{..} outDir topName extraGenerated = do
    let yosys :: String -> [String] -> Action ()
        yosys tool args = cmd_ (EchoStdout False) =<< toolchain "YOSYS" tool args

    let json = outDir </> topName <.> "json"

    outDir </> topName <.> "ys" %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs
        writeFileChanged out $ unlines
            [ printf "read_verilog %s" $ unwords (srcs <> verilogs)
            , printf "hierarchy -top %s" topName
            , printf "synth_ecp5 -json %s" json
            ]

    json %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs

        let ys = out -<.> "ys"
        need [ys]
        yosys "yosys" ["-q", ys]

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
