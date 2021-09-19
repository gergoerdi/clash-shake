module Clash.Shake.Intel (quartus) where

import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config

import Data.Functor ((<&>))

quartusSh :: String -> [String] -> Action ()
quartusSh script args = cmd_ =<< toolchain "QUARTUS" "quartus_sh" ("-t":script:args)

quartus :: String -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
quartus projectName _ outDir srcDir _ = do
    let fullPathWithExt ext = srcDir </> projectName <.> ext
    let quartusTclFile = fullPathWithExt "tcl"
    let outPof = outDir </> projectName <.> "pof"

    outPof %> \_ -> do
        -- Define dependencies - Quartus's entry-point is <projectName>.qpf
        need [quartusTclFile, fullPathWithExt "qpf", fullPathWithExt "qsf", fullPathWithExt "sdc"]

        -- Extract root from
        quartusSh quartusTclFile [outDir]

    pure $ SynthKit
        { bitfile = outPof
        , phonies =
            [ ("quartus"
              , need [quartusTclFile] >> quartusSh quartusTclFile [outDir]
              )
            ]
        }
