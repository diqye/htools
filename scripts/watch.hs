-- stack runghc --package time  --package process --package mtl --package http-client --package http-client-tls --package bytestring --package connection --package data-default-class --package parsec --package string-conversions --package aeson
{-# LANGUAGE OverloadedStrings #-}
import qualified Module.Log as Log
import qualified Module.Sender as Sender
import qualified Module.Monitor as Monitor
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Text.Parsec as P
import Control.Concurrent(forkIO)

main = do
  Log.stdBuffer
  manager <- HTTPS.newTlsManager
  let monit = Monitor.newMonitHttp manager 30 "http://www.scavc.edu.cn/zsjy1/zsw1/gg.htm" "四川航天职业学院-招生"
  let monitgg = Monitor.newMonitHttp manager 30 "http://www.scavc.com/xwzx/tzgg.htm" "四川航天职业学院-公告"
  -- forkIO $ monit (plist "http://www.scavc.edu.cn/zsjy1/zsw1/") action
  monitgg (plist "http://www.scavc.com/xwzx/") action
  where ggList (a,(b,c)) = 
          b ++ "\n" ++ a
        action (Left err) = do
          Log.err $ show $ err
          Sender.sendMeTime $ show err
          pure Monitor.MontiSleep
        action (Right list) = do
          Sender.sendMeTime $ unlines $ map ggList list
          pure Monitor.MonitContinue

testParsec :: P.Parsec String () [(String,String)]
testParsec = do
  consumeToString1 "<h1>"
  P.count 4 P.anyChar
  a <- consumeToString1 "</h1>"
  pure [(a,a)]

plist :: String -> P.Parsec String () [(String,(String,String))]
plist prefix = do
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
    pure (prefix++c,(a,b))


consumeToString1 :: String -> P.Parsec String () String
consumeToString1 str = do
  success <- successMatched P.<|> anyCharMatched
  if success then pure "" else (pure (:) <*> P.anyChar <*> consumeToString1 str)
  where successMatched = (P.try $ P.lookAhead $ P.string str) *> pure True
        anyCharMatched = (P.lookAhead $ P.anyChar) *> pure False