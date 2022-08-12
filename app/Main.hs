module Main where

import Control.Concurrent
import Control.Concurrent.Extra (once)
import Control.Monad (replicateM, void)
import qualified System.Directory as FS
import qualified System.FSNotify as FS
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import qualified System.Posix.Signals as Signals
import qualified System.Random as Random

main :: IO ()
main =
  withRandomTempDirectory $ \watchedDir -> do
    let testFile = watchedDir </> "testfile"
    writeFile testFile "foo"

    FS.withManager $ \mgr -> do
      removeWatchDir <- once <$> FS.watchDir mgr watchedDir (const True) print
      let signalset =
            Signals.CatchOnce $ do
              putStrLn "Stopping file watcher"
              removeWatchDir
              -- Cachix is gracefully quiting by continuing to upload binaries
              threadDelay (1 * 1000 * 1000)
              putStrLn "Stopped."
      void $ Signals.installHandler Signals.sigINT signalset Nothing
      void $ Signals.installHandler Signals.sigTERM signalset Nothing

      -- Test that file watching is working
      FS.removeFile testFile
      threadDelay (1 * 1000 * 1000)

      -- Please exit. Like, now.
      Signals.raiseSignal Signals.sigINT
      Signals.raiseSignal Signals.sigTERM

      threadDelay (2 * 1000 * 1000)

withRandomTempDirectory :: (FilePath -> IO ()) -> IO ()
withRandomTempDirectory action = do
  randomID <- replicateM 10 $ Random.randomRIO ('a', 'z')
  Temp.withSystemTempDirectory ("test." <> randomID) action
