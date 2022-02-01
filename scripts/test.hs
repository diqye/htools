-- stack runghc --package time  --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
import qualified Module.Test as Test
import qualified Module.Log as Log
main = do
  Log.err "err"
  putStrLn "hello"
  Test.someFunc