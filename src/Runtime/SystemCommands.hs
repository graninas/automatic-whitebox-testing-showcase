module Runtime.SystemCommands 
  (runCmd)
  where

import System.Process (shell, readCreateProcess)

runCmd cmd = readCreateProcess (shell cmd) ""