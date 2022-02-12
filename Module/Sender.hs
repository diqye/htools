{-# LANGUAGE OverloadedStrings,CPP #-}
module Module.Sender where

import qualified Module.Log as Log
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Data.Aeson as A
import Data.String.Conversions(cs)
import Text.Printf(printf)
import Control.Concurrent(forkIO)

sendMeTime = sendMe "CNiZ0sAGEiJBRFhRVE5JNzVRWEFFWEJISTNTRDRNVUpRQlBQWFpZUkVJIgQIARAC.SHAcPOvTWS4Ir8s8XVo8xA_X9qJx8nVmxsYJJlIoWsk"
sendMeXAU = sendMe "COzY1sAGEiJBRFhRVE5JNzVRWEFFWEJISTNTRDRNVUpRQlBQWFpZUkVJIgsIAhoHQlhBVVVTRA.Fe-RFWE0NvyYEb7ZeTti8ZtZu95JNdpFVh9vLRtfqx0"
sendMeBTC = sendMe "CKzg1sAGEiJBRFhRVE5JNzVRWEFFWEJISTNTRDRNVUpRQlBQWFpZUkVJIgsIAhoHQkJUQ1VTRA.68bZiLLMmbCjwX1NdBHInZmq0JcSf6s6jRE6kL8gdcI"
-- | Send up to 500 characters to my phone
sendMe token text' = do
  let text = take 500 text'
-- #ifdef darwin_HOST_OS
--   forkIO $ do
--     SP.callCommand $ printf "osascript -e \"set Volume 1.5\""
--     SP.callCommand $ printf "say -v Ting-Ting  \"%s\"" text
--     SP.callCommand $ printf "osascript -e \"set Volume 0\""
-- #endif
  forkIO $ do
    let requestObject = A.object 
          [ "token" A..= (token :: String)
          , "text" A..= (text :: String)
          , "sound" A..= (1::Int)
          ]
    let req = "https://api.chanify.net/v1/sender" 
          { HTTP.method = "POST"
          , HTTP.requestHeaders = [("Content-Type","application/json; charset=utf-8")]
          , HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode requestObject
          }
    manager <- HTTP.newManager HTTPS.tlsManagerSettings
    result <- pure HTTP.responseBody <*> HTTP.httpLbs req manager
    Log.trace $ printf "sendToMe::%s -> %s" text (cs result :: String)
  pure ()