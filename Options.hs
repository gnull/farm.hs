module Options where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { exploit    :: FilePath
  , args       :: [String]
  , targets    :: String
  , sumbit     :: String
  , jobs       :: Int
  , regex      :: String }

sample :: Parser Options
sample = Options
      <$> argument str
          ( metavar "PROGRAM"
         <> help "Exploit program" )
      <*> (many $ argument str $
            metavar "[ARG1 [ARG2 ...]]"
         <> help "Arguments for exploit PROGRAM")
      <*> strOption
          ( long "targets"
         <> short 't'
         <> help "File containing list of target IP addresses"
         <> metavar "FILE" )
      <*> strOption
          ( long "submit"
         <> short 's'
         <> help "Shell command to submit flags. This command will get flag in `flag` environment variable. E.g. `echo $flag`"
         <> metavar "COMMAND")
      <*> (read <$> strOption
           ( long "jobs"
          <> short 'j'
          <> help "Number of parallel running exploit jobs"
          <> metavar "N"))
      <*> strOption
          ( long "flagre"
         <> short 'r'
         <> help "POSIX regex for flag"
         <> metavar "REGEX")

parse :: IO (Options)
parse = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run an exploit on list of target hosts"
     <> header "Helper program for Attack-Defence CTF competitions" )
