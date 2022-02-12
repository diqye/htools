-- stack runghc --package http-client --package aeson --package string-conversions --package http-types --package bytestring
{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Data.Aeson as A
import Text.Printf(printf)
import qualified Module.Log as Log
import Data.String.Conversions(cs)
import Module.Request 
import qualified Data.ByteString.Lazy.Char8 as Char8

-- main = do
--   Log.stdBuffer
--   manager <- HTTPS.newTlsManager
--   let requestObject = A.object 
--         [ "channel" A..= ("BTC" :: String)
--         , "content" A..= ("nihaoya" :: String)
--         ]
--   let req = "http://localhost:8877/c" 
--         { HTTP.method = "POST"
--         , HTTP.requestHeaders = [("Content-Type","application/json; charset=utf-8"),("Authorization","1qaz2wsx")]
--         , HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode requestObject
--         }
--   result <- pure HTTP.responseBody <*> HTTP.httpLbs req manager
--   putStrLn $ printf "%s" (cs result :: String)

main = do
  let param = A.object 
        [ "channel" A..= ("XAU" :: String)
        , "content" A..= ("nihaoya" :: String)
        ]
  let req = bhUtf8json param 
        $ hAuthorization "1qaz2wsx"
        $ mpost "http://hk.diqye.com/speaker/c"
  r <- httpActionBody req
  Char8.putStrLn r