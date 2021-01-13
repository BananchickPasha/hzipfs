module Main where

import Fuse
import System.Fuse
import System.Environment ( getProgName, getArgs )
import System.Directory (makeAbsolute)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  when (length args < 2) $ error errMessage
  path <- makeAbsolute $ head args
  let fuseArgs = drop 1 args
  ops <- initOps path
  fuseRun prog fuseArgs ops defaultExceptionHandler


errMessage = "Usage: hzipfs <zipPath> <mountPath>"

  


