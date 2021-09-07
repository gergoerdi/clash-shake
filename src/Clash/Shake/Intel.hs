{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Clash.Shake.Intel (quartus) where

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

import Data.Functor ((<&>))

fromJustError :: String -> Maybe a -> a
fromJustError err Nothing = error err
fromJustError _ (Just a) = a

quartus :: ClashKit -> FilePath -> FilePath -> String -> Rules SynthKit
quartus _ outDir srcDir topName = do
    let fullPathWithExt ext = srcDir </> topName <.> ext

    srcDir </> topName <.> "tcl" %> \out -> do
        let template = $(TH.compileMustacheFile "template/intel/project.tcl.mustache")
        let values = object [ T.pack "output_dir" .= T.pack outDir ]
        writeFileChanged out $ TL.unpack $ renderMustache template values

    outDir </> topName <.> "pof" %> \_ -> do
        -- Define dependencies - Quartus's entry-point is <topName>.qpf
        let quartusProjectFile = fullPathWithExt "qpf"
        need [srcDir </> topName <.> "tcl", quartusProjectFile, fullPathWithExt "qsf", fullPathWithExt "sdc"]

        -- Extract root from
        root <- getConfig "QUARTUS_ROOT" <&> fromJustError "QUARTUS_ROOT must be defined"
        cmd_ (root </> "quartus_sh") "--flow" "compile" quartusProjectFile

    pure $ SynthKit
        { bitfile = outDir </> topName <.> "pof"
        , phonies = []
        }
