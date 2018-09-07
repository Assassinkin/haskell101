#!/usr/bin/env runhaskell

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main = 
  withManager $ \mgr -> do
  -- Start watching a job (in the background)
    watchDir
      mgr               --manager
      "."               --directory to watch
      (const True)      -- predicate
      print             -- action
    -- sleep forever (until interrupted)
    forever $ threadDelay 100000
