import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)
import qualified System.Environment as Env
import System.Log.FastLogger ()


-- instance Show TcpConnection where
--   show (TcpConnection srcIp dstIP srcPort dstPort) = "Src IP: " ++ srcIp
data MetricsSocket = MetricsSocket NetlinkSocket Word16

-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6

-- Sends a SockDiagRequest
-- expects INetDiag 
genQueryPacket :: Packet SockDiagRequest
genQueryPacket = let
  -- Mesge type / flags /seqNum /pid
  flags = (fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT)

  -- might be a trick with seqnum
  hdr = Header msgTypeSockDiag flags magicSeq 0
  -- IPPROTO_TCP = 6,
  -- -1 => ignore cookie content
  -- _cookie = [ maxBound :: Word32, maxBound :: Word32]
  _cookie = [ 0 :: Word32, 0 :: Word32]

  -- TODO hardcoded for now
  iperfSrcPort = 0
  iperfDstPort = 0
  -- iperfSrcPort = iperfHardcodedSrcPort
  -- iperfDstPort = 5201
  -- 4 Word32
  -- ipSrc = [ fromOctetsLE [ 127, 0, 0, 1], 0, 0, 0]
  -- ipDst = [ fromOctetsLE [ 127, 0, 0, 1], 0, 0, 0]
  ipSrc = [ 0, 0, 0, 0]
  ipDst = [ 0, 0, 0, 0]
  -- 1 => "lo". Check with ip link ?
  ifIndex = 0
  diag_req = InetDiagSockId iperfSrcPort iperfDstPort ipSrc ipDst ifIndex _cookie

    -- #define SS_ALL ((1 << SS_MAX) - 1)
    -- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
  stateFilter = [TcpEstablished]
  -- stateFilter = [TcpListen, TcpEstablished, TcpSynSent ]
  -- InetDiagInfo
  requestedInfo = InetDiagNone
  padding = 0 -- useless
  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo padding (stateFilter) diag_req
  in
     Packet hdr custom Map.empty


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
