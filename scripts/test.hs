-- stack repl --package lens --package template-haskell --package time --package text  --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
import Module.Log(hInfo,info')
import Data.Text (Text)
import Language.Haskell.TH
import System.IO
import Data.Aeson
import Control.Monad
import qualified Module.Test as Test
import Module.TemplateGetter

data Foo a = Foo
  { field_1 :: String
  , field_2:: a
  } deriving Show

foo = Foo "ddd" 123
mkSetAndOver ''Foo

main = do
  -- Test.testLog'
  putStrLn "11" >> $info' "hello"
  let foo' = set_field_2 0 
        $ set_field_1 "000"
        $ foo
  print foo'
  pure ()
