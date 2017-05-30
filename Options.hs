{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Options where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

data Options' = Options'
  { exploit'   :: FilePath
  , args'      :: [String]
  , targets'   :: String
  , sumbit'    :: String
  , jobs'      :: Int
  , delay'     :: Int
  , regex'     :: String }

data Options = Options
  { exploit    :: FilePath
  , args       :: [String]
  , targets    :: [String]
  , sumbit     :: String
  , jobs       :: Int
  , delay      :: Int
  , regex      :: String }

sample :: Parser Options'
sample = do
   exploit' <- argument str $
            metavar "PROGRAM"
         <> help "Exploit program"
   args' <- many $ argument str $
            metavar "[ARG1 [ARG2 ...]]"
         <> help "Arguments for exploit PROGRAM"
   targets' <- strOption $
            long "targets"
         <> short 't'
         <> help "File containing list of target IP addresses"
         <> metavar "FILE"
   sumbit' <- strOption $
            long "submit"
         <> short 's'
         <> help "Shell command to submit flags. This command will get flag in `flag` environment variable. E.g. `echo $flag`"
         <> metavar "COMMAND"
   jobs' <- fmap read $ strOption $
            long "jobs"
         <> short 'j'
         <> help "Number of parallel running exploit jobs"
         <> metavar "N"
   delay' <- fmap (fromMaybe 0) $ optional $ fmap read $ strOption $
            long "delay"
         <> short 'd'
         <> help "Number of seconds to wait after each exploit run (defaults to 0)"
         <> metavar "SECS"
   regex' <- strOption $
            long "flagre"
         <> short 'r'
         <> help "POSIX regex for flag"
         <> metavar "REGEX"
   pure Options' {..}

checkOptions :: Options' -> IO Options
checkOptions o = do
  let exploit = exploit' o
  let args = args' o
  targets <- lines <$> readFile (targets' o)
  let sumbit = sumbit' o
  let jobs = min (length targets) (jobs' o)
  let delay = delay' o
  let regex = regex' o
  return Options {..}

parse :: IO (Options)
parse = execParser opts >>= checkOptions
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run an exploit on list of target hosts"
     <> header "Helper program for Attack-Defence CTF competitions" )
