import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)
import qualified System.Environment as Env
import System.Log.FastLogger ()
import Data.Word (Word8, Word16, Word32)
import Mptcp


-- instance Show TcpConnection where
--   show (TcpConnection srcIp dstIP srcPort dstPort) = "Src IP: " ++ srcIp
data MetricsSocket = MetricsSocket NetlinkSocket Word16



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
  case Env.getEnv "token" of
    Nothing -> error "Needs the token of the connection to monitor"
    Just token -> putStrLn "Monitoring token " ++ show token
  putStrLn $ "Starting monitoring connection with token " ++ show token
  (MptcpSocket sock  fid) <- makeMptcpSocket
  putStr "socket created. MPTCP Family id " >> print fid
  let mptcpConnection = MptcpConnection token []

  -- (sockMetrics, fidMetrics) <- makeMetricsSocket
  putStrLn "Creating metrics netlink socket..."
  sockMetrics <- makeMetricsSocket

  mapM_ (listenToEvents (MptcpSocket sock fid)) mcastMptcpGroups

  -- sendPacket
  sendPacket sockMetrics genQueryPacket >> putStrLn "Sent the TCP SS request"

  -- exported from my own version !!
  recvMulti sockMetrics >>= inspectIdiagAnswers
