module Options where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { exploit    :: FilePath
  , targets    :: String }

sample :: Parser Options
sample = Options
      <$> argument str
          ( metavar "PROGRAM"
         <> help "Exploit program" )
      <*> strOption
          ( long "targets"
         <> help "File containing list of target IP addresses"
         <> metavar "FILE" )

parse :: IO (Options)
parse = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run an exploit on list of target hosts"
     <> header "Helper program for Attack-Defence CTF competitions" )
