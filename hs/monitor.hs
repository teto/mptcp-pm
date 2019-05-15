import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)

import System.Log.FastLogger ()

data Sample = Sample
  { command    :: String
  , quiet      :: Bool
  , enthusiasm :: Int }


-- TODO register subcommands instead
sample :: Parser Sample
sample = Sample
      <$> argument str
          ( metavar "CMD"
         <> help "What to do" )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> Options.Applicative.value 1
         <> metavar "INT" )


opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  putStrLn "Starting monitor"

