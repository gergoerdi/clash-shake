{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clash.Shake.F4PGA
    ( xilinx7
    , openFPGALoader
    , yosysRules
    , yosys
    ) where

import Clash.Shake
import qualified Clash.Shake.Xilinx as Xilinx

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Text.Printf (printf)

xilinx7 :: Xilinx.Board -> SynthRules
xilinx7 Xilinx.Board{ boardTarget = target@Xilinx.Target{..} } kit@ClashKit{..} outDir topName extraGenerated = do
    let rootDir = joinPath . map (const "..") . splitPath $ outDir

    let symbiflow' :: String -> [String] -> Action ()
        symbiflow' tool args = cmd_ (EchoStdout False) (Cwd outDir) =<< toolchain "F4PGA" tool args
        symbiflow :: String -> [String] -> Action ()
        symbiflow tool args = cmd_ (EchoStdout False) =<< toolchain "F4PGA" tool args

    let produce ext = outDir </> topName <.> ext

    produce "eblif" %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
            xdcs = extraFiles ["//*.xdc"]
        need $ srcs <> verilogs <> xdcs

        symbiflow' "symbiflow_synth" $
          [ "-d", targetFamily
          , "-p", Xilinx.targetPart target
          , "-t", topName
          ] ++
          [ "-v " <> rootDir </> src | src <- srcs <> verilogs ] ++
          [ "-x " <> rootDir </> xdc | xdc <- xdcs ]

    produce "net" %> \out -> do
        let eblif = out -<.> "eblif"
        need [eblif]
        symbiflow' "symbiflow_pack" $
          [ "-d", targetDevice <> "_test"
          , "-e", takeFileName eblif
          ]

    produce "place" %> \out -> do
        let eblif = out -<.> "eblif"
            net = out -<.> "net"
        need [eblif, net]
        symbiflow' "symbiflow_place" $
          [ "-d", targetDevice <> "_test"
          , "-P", Xilinx.targetPart target
          , "-e", takeFileName eblif
          , "-n", takeFileName net
          ]

    produce "route" %> \out -> do
        let eblif = out -<.> "eblif"
            place = out -<.> "place"
        need [eblif, place]
        symbiflow' "symbiflow_route" $
          [ "-d", targetDevice <> "_test"
          , "-e", takeFileName eblif
          ]

    produce "fasm" %> \out -> do
        let eblif = out -<.> "eblif"
            route = out -<.> "route"
        need [eblif, route]
        symbiflow' "symbiflow_write_fasm" $
          [ "-d", targetDevice <> "_test"
          , "-e", takeFileName eblif
          ]

    produce "bit" %> \out -> do
        let fasm = out -<.> "fasm"
        need [fasm]
        symbiflow "symbiflow_write_bitstream" $
          [ "-d", targetFamily
          , "-p", Xilinx.targetPart target
          , "-f", fasm
          , "-b", out
          ]

    let bitfile = outDir </> topName <.> "bit"

    return SynthKit
        { bitfile = bitfile
        , phonies =
            [ "upload" |> openFPGALoader ["-c", "digilent"] bitfile
            ]
        }

yosysRules :: ClashKit -> FilePath -> String -> Action [FilePath] -> String -> Rules FilePath
yosysRules ClashKit{..} outDir topName extraGenerated synth = do
    outDir </> topName <.> "ys" %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs
        writeFileChanged out $
            unlines
                [ printf "read_verilog %s" $ unwords (srcs <> verilogs)
                , printf "hierarchy -top %s" topName
                , printf "synth_%s -json %s" synth json
                ]

    json %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        srcs <- manifestSrcs
        let verilogs = extraFiles ["//*.v"]
        need $ srcs <> verilogs

        let ys = out -<.> "ys"
        need [ys]
        yosys "yosys" ["-q", ys]
    pure json
  where
    json = outDir </> topName <.> "json"

yosys :: String -> [String] -> Action ()
yosys tool args = cmd_ (EchoStdout False) =<< toolchain "YOSYS" tool args

openFPGALoader :: [String] -> FilePath -> Action ()
openFPGALoader args bitfile = do
    need [bitfile]
    cmd_ =<< toolchain "OPENFPGALOADER" "openFPGALoader" (args ++ [bitfile])
