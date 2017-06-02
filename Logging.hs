module Logging (showLog, showLogPlain, LogMsg (..)) where

-- Maybe I should use https://hackage.haskell.org/package/ansi-wl-pprint ?
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen
import System.Exit (ExitCode (..))

data LogMsg = ExplMsg [String] ExitCode String String [String]
            | SubmMsg String ExitCode String String

heading :: String -> Doc -> Doc
heading h x = indent 2 $ (cyan $ text h) <> colon <+> align x

title :: Show a => String -> a -> ExitCode -> Doc
title t r e = cyan (underline $ text t <+> (text . show) r <> colon) <+> exitCode e

exitCode :: ExitCode -> Doc
exitCode x@ExitSuccess = text $ show x
exitCode x@(ExitFailure _) = red $ text $ show x

logDoc :: LogMsg -> Doc
logDoc (ExplMsg cmd ret out err fs) = title "Exploit" cmd ret
  <$> heading "stdout" (string out)
  <$> heading "stderr" (string err)
  <$> heading "flags" (green $ align $ fillSep $ map (string . show) fs)
  <$> line
logDoc (SubmMsg cmd ret out err) = title "Submit" cmd ret
  <$> heading "stdout" (string out)
  <$> heading "stderr" (string err)
  <$> line

showLog :: LogMsg -> IO ()
showLog = putDoc . logDoc

showLogPlain :: LogMsg -> IO ()
showLogPlain = putDoc . plain . logDoc
