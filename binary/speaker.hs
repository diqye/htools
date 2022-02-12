-- stack repl --package time --package telegram-api --package appm --package unordered-containers --package text --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
{-# LANGUAGE OverloadedStrings #-}
import qualified Module.Log as Log
import qualified Web.AppM as A
import qualified Data.Text as T
import qualified Module.FreeJSON as FJ
import qualified Module.Sender as Sender
import Control.Exception(displayException)
import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions(cs)
import qualified Web.Telegram.API.Bot as B
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Data.ByteString.Lazy.Char8 as Char8


main = do
  Log.stdBuffer
  A.runSettings setting $ A.toApplication $ app

setting = A.setPort 8877
  $ A.setOnException (\ _ e -> Log.err $ ("**OnException:\n" ++) $ displayException e)
  $ A.setOnExceptionResponse A.exceptionResponseForDebug
  $ A.setTimeout (30*60*60)
  $ A.defaultSettings

auth :: Monad m => A.AppT m A.Application -> A.AppT m A.Application
auth app = do
  req <- A.getRequest
  let auth = lookup "Authorization" $ A.requestHeaders req
  if auth == Just "1qaz2wsx" then app
  else A.respLBS A.status401 "Authorization is invalid"

app :: A.AppIO
app = auth $ A.appmsum
 [ A.consum "c" >> cspeakder
 , A.consum "t" >> tspeakder
 , A.home >> A.respLBS A.status200 "ok"
 ]


cspeakderHelper :: FJ.Value -> Either (Int,String) (String,String) 
cspeakderHelper value = do
  channel <- FJ.getField "channel" value
  content <- FJ.getField "content" value
  pure (channel,content)

tspeakder :: A.AppIO
tspeakder = do
  body <- A.bodyJSONV
  case cspeakderHelper body of
    (Left err) -> A.respLBS A.status500 $ FJ.encode err
    (Right ("XAU",content)) -> do
      liftIO $ runClient $ sendTgXAU $ cs content
      A.respLBS A.status200 $ "ok"
    (Right (channel,content)) -> do
      liftIO $ runClient $ sendTgXAU $ cs (channel++":"++content)
      A.respLBS A.status200 $ "ok"

cspeakder :: A.AppIO
cspeakder = do
  body <- A.bodyJSONV
  case cspeakderHelper body of
    (Left err) -> A.respLBS A.status500 $ FJ.encode err
    (Right ("XAU",content)) -> do
      liftIO $ Sender.sendMeXAU content
      A.respLBS A.status200 $ "ok"
    (Right ("Time",content)) -> do
      liftIO $ Sender.sendMeTime content
      A.respLBS A.status200 $ "ok"
    (Right ("BTC",content)) -> do
      liftIO $ Sender.sendMeBTC content
      A.respLBS A.status200 $ "ok"
    _ -> A.respLBS A.status500 $ FJ.encode (404::Int,"channel is not exist"::String)


runClient :: B.TelegramClient a -> IO ()
runClient clientMonad = do
  let token = B.Token "bot1449676439:AAHBpnIYURQT237qU4U01R2Lct3twjDm1ZA" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- HTTPS.newTlsManager
  _ <- B.runTelegramClient token manager clientMonad
  pure ()

sendTgXAU :: T.Text -> B.TelegramClient ()
sendTgXAU text = do
   let msg = B.sendMessageRequest (B.ChatId (-1001605703794)) text
   B.sendMessageM msg
   pure ()

sendTgMe:: T.Text -> B.TelegramClient ()
sendTgMe text = do
   let msg = B.sendMessageRequest (B.ChatId 1129803474) text
   B.sendMessageM msg
   pure ()

updatetest :: IO ()
updatetest = runClient $ do
  (B.Response updates _) <- B.getUpdatesM $ B.GetUpdatesRequest (Just 0) Nothing (Just 20) Nothing
  liftIO $ Char8.putStrLn $ FJ.encode updates