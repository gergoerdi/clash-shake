{-# LANGUAGE RecordWildCards #-}
module Clash.Shake.Symbiflow where

import           Clash.Shake
import           Development.Shake
import           Development.Shake.FilePath

data SymbiflowTarget = SymbiflowTarget
  { targetSize    :: Word
  , targetPackage :: String
  , targetSpeed   :: Word
  , targetFreq    :: Word
  }

iCESugar_Pro_v1_3 :: SymbiflowTarget
iCESugar_Pro_v1_3 = SymbiflowTarget { targetSize    = 25
                                    , targetPackage = "CABGA256"
                                    , targetSpeed   = 6
                                    , targetFreq    = 65
                                    }

symbiflowECP5
  :: SymbiflowTarget
  -> ClashKit
  -> FilePath
  -> FilePath
  -> String
  -> Rules SynthKit
symbiflowECP5 SymbiflowTarget {..} kit@ClashKit {..} outDir srcDir topName = do
  let getFiles dir pats =
        getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
      hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v"]

  outDir </> "*.json" %> \out -> do
    srcs1 <- fmap (srcDir </>) <$> hdlSrcs
    srcs2 <- manifestSrcs
    need (srcs1 <> srcs2)
    cmd_ "yosys"
         ["-p", "synth_ecp5 -json " <> out]
         srcs1
         srcs2

  outDir </> "*.config" %> \out -> do
    let json = out -<.> "json"
        lpf  = srcDir </> takeFileName out -<.> "lpf"
    need [json, lpf]
    cmd_ "nextpnr-ecp5"
         ["--" <> show targetSize <> "k"]
         ["--package", targetPackage]
         ["--speed", show targetSpeed]
         ["--json", json]
         ["--textcfg", out]
         ["--lpf", lpf]
         ["--freq", show targetFreq]

  outDir </> "*.bit" %> \out -> do
    let config = out -<.> "config"
    need [config]
    cmd_ "ecppack" [config, out]

  let bf = outDir </> topName <.> "bit"
  pure SynthKit
    { bitfile = bf
    , phonies = [ "program" |> do
                    need [bf]
                    cmd_ "ecpdap" ["program", bf]
                ]
    }

(|>) :: String -> Action () -> (String, Action ())
(|>) = (,)
