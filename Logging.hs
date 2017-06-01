module Logging (showLog, LogMsg (..)) where

import System.Exit (ExitCode)

data LogMsg = ExplMsg [String] ExitCode String String [String]
            | SubmMsg String ExitCode String String

showLog' :: Show a => a -> (ExitCode, String, String) -> [String] -> String
showLog' cmd (ret, out, err) flags = unlines
  $  [show cmd ++ " returned " ++ show ret]
  ++ map ("  stderr: " ++) (lines err)
  ++ map ("  stdout: " ++) (lines out)
  ++ map ("    flag: " ++) flags

showLog :: LogMsg -> String
showLog (ExplMsg cmd ret out err fs) = showLog' cmd (ret, out, err) fs
showLog (SubmMsg cmd ret out err   ) = showLog' cmd (ret, out, err) []
