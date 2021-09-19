{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Clash.Shake.Intel (quartus) where

import Clash.Shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config

import Data.Functor ((<&>))

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T

quartusSh :: String -> [String] -> Action ()
quartusSh script args = do
    need [script]
    cmd_ =<< toolchain "QUARTUS" "quartus_sh" ("-t":script:args)

quartus :: String -> ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
quartus projectName ClashKit{..} outDir srcDir topName = do
    let projectDir = outDir </> projectName
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let fullPathWithExt ext = srcDir </> projectName <.> ext
    let quartusTclFile = fullPathWithExt "tcl"
    let outPof = outDir </> projectName <.> "pof"

    outDir </> projectName <.> "qsf" %> \out -> do
        clashSrcs <- manifestSrcs
        targetSrcs <- map (srcDir </>) <$> getDirectoryFiles srcDir ["*.vhdl", "*.v" ]
        qips <- map (srcDir </>) <$> getDirectoryFiles srcDir ["*.qip" ]
        qsfs <- map (srcDir </>) <$> getDirectoryFiles srcDir ["*.qsf" ]

        let template = $(TH.compileMustacheFile "template/intel/project.qsf.mustache")
            values = object . mconcat $
                     [ [ "project" .= T.pack projectName ]
                     , [ "top" .= T.pack topName ]
                     , [ "srcs" .= mconcat
                         [ [ object [ "fileName" .= (rootDir </> src) ] | src <- clashSrcs ]
                         , [ object [ "fileName" .= (rootDir </> src) ] | src <- targetSrcs ]
                         ]
                       ]
                     , [ "qips" .= [ object [ "fileName" .= (rootDir </> qip) ] | qip <- qips ] ]
                     ]

        let files = renderMustache template values
        qsfs <- mapM readFile' qsfs
        writeFileChanged out $ TL.unpack files <> mconcat qsfs

    outPof %> \_ -> do
        srcs <- manifestSrcs
        -- Define dependencies - Quartus's entry-point is <projectName>.qpf
        need $ mconcat
          [ srcs
          , [outDir </> projectName <.> "qsf", fullPathWithExt "qpf", fullPathWithExt "sdc"]
          ]

        -- Extract root from
        quartusSh quartusTclFile [outDir]

    pure $ SynthKit
        { bitfile = outPof
        , phonies =
            [ ("quartus"
              , quartusSh quartusTclFile [outDir]
              )
            ]
        }
