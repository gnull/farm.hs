module Logging (showLog, showLogColor, LogMsg (..)) where

-- Maybe I should use https://hackage.haskell.org/package/ansi-wl-pprint ?
import System.Console.ANSI
import System.Exit (ExitCode)

data LogMsg = ExplMsg [String] ExitCode String String [String]
            | SubmMsg String ExitCode String String

showLog' :: Show a => a -> (ExitCode, String, String) -> [String] -> String
showLog' cmd (ret, out, err) flags = unlines
  $  [show cmd ++ " returned " ++ show ret]
  ++ map ("  stderr: " ++) (lines err)
  ++ map ("  stdout: " ++) (lines out)
  ++ map ("    flag: " ++) flags

stdoutCode = setSGRCode [SetColor Foreground Vivid Yellow]
stderrCode = setSGRCode [SetColor Foreground Vivid Magenta]
flagsCode = setSGRCode [SetColor Foreground Vivid Green, SetBlinkSpeed SlowBlink]
resetCode = setSGRCode [Reset]

showLogColor' :: Show a => a -> (ExitCode, String, String) -> [String] -> String
showLogColor' cmd (ret, out, err) flags = unlines
  $  [show cmd ++ " returned " ++ show ret]
  ++ map (("  stderr: " ++) . (stdoutCode ++) . (++ resetCode)) (lines err)
  ++ map (("  stdout: " ++) . (stderrCode ++) . (++ resetCode)) (lines out)
  ++ map (("    flag: " ++) . (flagsCode ++) . (++ resetCode)) flags

showLog :: LogMsg -> String
showLog (ExplMsg cmd ret out err fs) = showLog' cmd (ret, out, err) fs
showLog (SubmMsg cmd ret out err   ) = showLog' cmd (ret, out, err) []

showLogColor :: LogMsg -> String
showLogColor (ExplMsg cmd ret out err fs) = showLogColor' cmd (ret, out, err) fs
showLogColor (SubmMsg cmd ret out err   ) = showLogColor' cmd (ret, out, err) []
