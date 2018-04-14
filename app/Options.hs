{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Options (parse, Options (..)) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

data Options = Options
  { exploit   :: FilePath
  , args      :: [String]
  , targets   :: String
  , submit    :: String
  , jobs      :: Maybe Int
  , regex     :: String
  , color     :: Bool
  }

sample :: Parser Options
sample = do
   exploit <- argument str $
            metavar "PROGRAM"
         <> help "Exploit program"
   args <- many $ argument str $
            metavar "[ARG1 [ARG2 ...]]"
         <> help "Arguments for exploit PROGRAM"
   targets <- strOption $
            long "targets"
         <> short 't'
         <> help "File containing list of target IP addresses"
         <> metavar "FILE"
   submit <- strOption $
            long "submit"
         <> short 's'
         <> help "Shell command to submit flags. This command will get flag in `flag` environment variable. E.g. `echo $flag`"
         <> metavar "COMMAND"
   jobs <- fmap (read <$>) $ optional $ strOption $
            long "jobs"
         <> short 'j'
         <> help "Number of parallel running exploit jobs"
         <> metavar "N"
   regex <- strOption $
            long "flagre"
         <> short 'r'
         <> help "POSIX regex for flag"
         <> metavar "REGEX"
   color <- fmap not $ switch $
            long "nocolor"
         <> short 'c'
         <> help "Disable colors in output"
   pure Options {..}

parse :: IO (Options)
parse = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run an exploit on list of target hosts"
     <> header "Helper program for Attack-Defence CTF competitions"
     <> footer ( "EXAMPLE: "
              ++ "farm-hs -t <(for i in $(seq 100); do echo \"192.168.34.$i\"; done;) -s 'echo $flag | ncat jury.ctf.com 8080' -j 10 -r 'flag\\{\\w+\\}|galf\\{\\w+\\}' -- ./my-exploit.sh"
               )
      )
