{-# LANGUAGE OverloadedStrings,CPP #-}
module Module.Monitor where
import qualified Module.Log as Log
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Text.Parsec as P
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad(guard,when,unless,forever)
import Control.Monad.IO.Class(liftIO)
import Data.List((\\))
import Data.String.Conversions(cs)
import Control.Monad.Trans.Maybe(runMaybeT)

oneSec = fromEnum 1e6 :: Int

data MonitControl = MonitContinue | MontiSleep | MonitEnd deriving Show
-- | For example; 60s
-- let manager <- HTTPS.newTlsManager 
-- let monitHttp = newMonitHttp manager 60 "https://google.com" "exampel"
-- monitHttp htmlparsec $ \ r -> some code...
newMonitHttp :: HTTP.Manager
  -> Int
  -> HTTP.Request
  -> String -- new end
  -> P.Parsec String () [(String,a)]
  -> (Either P.ParseError [(String,a)] -> IO MonitControl) 
  -> IO ()
newMonitHttp manager await request name parsec action = do
  mvar <- newEmptyMVar 
  Log.info $ name ++ " watching is stared"
  monitHttp mvar
  where monitHttp mvar = do
          text <- HTTP.responseBody <$> HTTP.httpLbs request manager
          let parsecR = P.parse parsec name $ cs text
          control <- case parsecR of
            Right list -> runMaybeT $ do
              guard $ not $ null list
              isEmptyMVar' <- liftIO $ isEmptyMVar mvar
              let idList = map fst list
              when isEmptyMVar' $ do
                liftIO $ putMVar mvar idList
                guard True
              lastIdList <- liftIO $ swapMVar mvar idList
              let newIdList = idList \\ lastIdList
              guard $ not $ null newIdList
              liftIO $ action $ fmap (const (filter (byId newIdList) list)) parsecR
            Left _ ->  do
              a <- action parsecR
              pure $ Just a
          case control of
            Nothing -> do
              threadDelay (await * oneSec)
              monitHttp mvar
            (Just MonitContinue) -> do
              threadDelay (await * oneSec)
              monitHttp mvar
            (Just MonitEnd) -> do
              Log.info $ name ++ " ended"
              pure ()
            (Just MontiSleep) ->do
              Log.info $ name ++ " sleeped"
              forever $ threadDelay (10000*oneSec)
        byId idList (id,a) = not $ null $ filter (==id) idList
              
debugParsec :: HTTP.Manager -> P.Parsec String () [(String,a)] -> HTTP.Request -> IO (Either P.ParseError [(String,a)])
debugParsec manager parsec request = do
  text <- HTTP.responseBody <$> HTTP.httpLbs request manager
  let parsecR = P.parse parsec "debug" $ cs text
  pure parsecR
