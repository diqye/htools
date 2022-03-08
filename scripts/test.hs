-- stack repl --package lens --package template-haskell --package time --package text  --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
import Module.Log(hInfo,info')
import Data.Text (Text)
import Language.Haskell.TH
import System.IO
import GHC.Generics
import Data.Aeson
import Control.Monad
import qualified Module.Test as Test


main = do
  Test.testLog'
  pure ()

