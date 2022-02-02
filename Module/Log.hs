module Module.Log where

import System.IO
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.Time.Format
import Data.Monoid((<>))
import qualified Data.Time as Time
import System.IO

data LogLevel = LogErr | LogInfo | LogTrace

-- China standard time
cstZone = Time.TimeZone {Time.timeZoneMinutes=480,Time.timeZoneSummerOnly=False,Time.timeZoneName="CST"}

stdBuffer = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

hInfo :: (MonadIO m) => Handle -> LogLevel -> String -> m ()
hInfo handle level str = liftIO $ do
  utc <- Time.getCurrentTime
  let zone = cstZone
  let time = Time.utcToLocalTime zone utc
  let prefix = case level of
        LogErr -> "?"
        LogTrace -> "!"
        LogInfo -> " "
  hPutStrLn handle $ (formatTime defaultTimeLocale ("%Y-%m-%d %H:%M:%S>" <> prefix) time <> str)

info :: (MonadIO m) => String -> m ()
info = hInfo stdout LogInfo

trace :: (MonadIO m) => String -> m ()
trace = hInfo stdout LogTrace


err :: (MonadIO m) => String -> m ()
err = hInfo stderr LogErr