module Module.Log where

import System.IO
-- import Text.Printf(printf,hPrintf)
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.Time.Format
import Data.Monoid((<>))
import qualified Data.Time as Time

data LogLevel = LogErr | LogInfo | LogTrace

hInfo :: (MonadIO m) => Handle -> LogLevel -> String -> m ()
hInfo handle level str = liftIO $ do
  let cstZone = Time.TimeZone {Time.timeZoneMinutes=480,Time.timeZoneSummerOnly=False,Time.timeZoneName="CST"}
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