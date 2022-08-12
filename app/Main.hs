module Main where

import Control.Concurrent
import Control.Exception (bracket)
import qualified Control.Exception
import Control.Monad (replicateM, void)
import qualified System.Directory as FS
import qualified System.FSNotify as FS
import System.FilePath (FilePath, (</>))
import qualified System.IO.Temp as Temp
import qualified System.Random as Random

main :: IO ()
main =
  withRandomTempDirectory $ \watchedDir -> do
    let testDir = watchedDir </> "test"
    FS.createDirectory testDir
    writeFile (testDir </> "testfile") "boop"

    void . forkIO $
      FS.withManager $ \mgr ->
        bracket
          (FS.watchDir mgr watchedDir (const True) print)
          void
          id

    FS.removeDirectoryRecursive testDir

    threadDelay (2 * 1000 * 1000)

withRandomTempDirectory :: (FilePath -> IO ()) -> IO ()
withRandomTempDirectory action = do
  randomID <- replicateM 10 $ Random.randomRIO ('a', 'z')
  Temp.withSystemTempDirectory ("test." <> randomID) action
