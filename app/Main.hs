module Main where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe
import Control.Monad (forever, replicateM, void)
import qualified System.Directory as FS
import qualified System.FSNotify as FS
import System.FilePath (FilePath, (</>))
import qualified System.IO.Temp as Temp
import qualified System.Posix.Signals as Signals
import qualified System.Random as Random

main :: IO ()
main =
  withRandomTempDirectory $ \watchedDir -> do
    FS.withManager $ \mgr -> do
      stop <- FS.watchDir mgr watchedDir (const True) print
      let signalset = Signals.CatchOnce stop
      void $ Signals.installHandler Signals.sigINT signalset Nothing
      void $ Signals.installHandler Signals.sigTERM signalset Nothing

    Async.concurrently (Signals.raiseSignal Signals.sigINT) (Signals.raiseSignal Signals.sigTERM)

    threadDelay (2 * 1000 * 1000)

withRandomTempDirectory :: (FilePath -> IO ()) -> IO ()
withRandomTempDirectory action = do
  randomID <- replicateM 10 $ Random.randomRIO ('a', 'z')
  Temp.withSystemTempDirectory ("test." <> randomID) action
