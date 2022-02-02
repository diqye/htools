-- stack ghci --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
{-# LANGUAGE OverloadedStrings,CPP #-}
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Network.Connection as NC
import Data.Default.Class
import Data.String.Conversions(cs)
import qualified Data.Aeson as A
import qualified Text.Parsec as P
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad(forM_)
import Control.Monad
import qualified System.Process as SP
import Text.Printf(printf)
import Control.Exception
import System.IO


oneSec = fromEnum 1e6 :: Int

main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  manager <- HTTPS.newTlsManager
  last <- newEmptyMVar
  forever $ loopTask last manager `catch` (\e->print (e::SomeException))
loopTask last manager= do
  threadDelay (5 * oneSec)
  text <- pure HTTP.responseBody <*> HTTP.httpLbs "http://www.scavc.edu.cn/zsjy1/zsw1/gg.htm" manager
  let list = P.parse plist "scavc.edu.cn" $ cs text
  case list of
    Right ls -> do
      isEmpty <- isEmptyMVar last 
      when isEmpty $ do
        let (_,_,id) = head ls
        putMVar last id
        pure ()
      id <- readMVar last
      let newLs = takeWhile (\(_,_,id')->id'/=id) ls
      if null newLs then do
        putStrLn "No new paper"
      else do
        let (_,_,id) = head newLs
        swapMVar last id 
        forM_  newLs $ \ (a,_,id)-> do 
          putStrLn a
          sendMeTime (a ++ "\n http://www.scavc.edu.cn/zsjy1/zsw1/" ++ id)
    Left e -> do
      putStrLn $ show e
  threadDelay (30 * oneSec)

plist :: P.Parsec String () [(String,String,String)]
plist = do
  consumeToString1 "ej_list_list"
  P.many $ P.try $ do
    consumeToString1 "<a"
    consumeToString1 "href=\""
    P.count 6 P.anyChar
    c <- P.many $ P.noneOf "\""
    consumeToString1 ">"
    P.char '>'
    a <- consumeToString1 "<span"
    consumeToString1 "date_list"
    P.count 11 P.anyChar
    b <- consumeToString1 "</SPAN>"
    pure (a,b,c)


consumeToString1 :: String -> P.Parsec String () String
consumeToString1 str = do
  success <- successMatched P.<|> anyCharMatched
  if success then pure "" else (pure (:) <*> P.anyChar <*> consumeToString1 str)
  where successMatched = (P.try $ P.lookAhead $ P.string str) *> pure True
        anyCharMatched = (P.lookAhead $ P.anyChar) *> pure False



sendMeTime = sendMe "CNiZ0sAGEiJBRFhRVE5JNzVRWEFFWEJISTNTRDRNVUpRQlBQWFpZUkVJIgQIARAC.SHAcPOvTWS4Ir8s8XVo8xA_X9qJx8nVmxsYJJlIoWsk"

sendMe token text = do
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
    putStrLn $ printf "sendToMe::%s -> %s" text (cs result :: String)
  pure ()
