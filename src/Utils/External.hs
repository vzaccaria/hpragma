
module Utils.External where

import System.IO
  ( hGetContents )
import System.Process
  ( runInteractiveCommand
  , waitForProcess
  )
import System.Exit
  ( ExitCode ( .. ) )

getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command =
     -- Create the process
  do (_pIn, pOut, pErr, handle) <- runInteractiveCommand command
     -- Wait for the process to finish and store its exit code
     exitCode <- waitForProcess handle
     -- Get the standard output.
     output   <- hGetContents pOut
     -- return both the output and the exit code.
     return (output, exitCode)
