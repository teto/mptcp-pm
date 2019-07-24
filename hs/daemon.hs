{-|
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

Monitors new MPTCP connections and runs a specific monitor instance

To interact
GENL_ADMIN_PERM
The operation requires the CAP_NET_ADMIN privilege

Helpful links:

- inspired by https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
- super nice tutorial on optparse applicative: https://github.com/pcapriotti/optparse-applicative#regular-options
- How to use Map https://lotz84.github.io/haskellbyexample/ex/maps
= How to update a single field https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
- get tcp stats via netlink https://www.vividcortex.com/blog/2014/09/22/using-netlink-to-optimize-socket-statistics/
eNETLINK_INET_DIAG


Concurrent values
- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch07.html
- https://stackoverflow.com/questions/22171895/using-tchan-with-timeout
- https://www.reddit.com/r/haskellquestions/comments/5rh1d4/concurrency_and_shared_state/

Mutable states:
- https://blog.jakuba.net/2014/07/20/Mutable-State-in-Haskell/
- http://mainisusuallyafunction.blogspot.com/2011/10/safe-top-level-mutable-variables-for.html

http://kristrev.github.io/2013/07/26/passive-monitoring-of-sockets-on-linux

iproute2/misc/ss.c to see how `ss` utility interacts with the kernel
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/process-1.6.5.0/System-Process.html

Aeson tutorials:
-- https://haskell.fpcomplete.com/library/aeson
- https://lotz84.github.io/haskellbyexample/ex/timers

- https://lotz84.github.io/haskellbyexample/ex/maps


Useful functions in Map
- member / elemes / keys / assocs / keysSet / toList
-}

-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (concat, init)
import Options.Applicative hiding (value, ErrorMsg, empty)
import qualified Options.Applicative (value)

-- For TcpState, FFI generated
import Generated
import Net.SockDiag
import Net.Mptcp
import Net.Mptcp.PathManager
import Net.Tcp
import Net.IP
import Net.IPv4 hiding (print)
import Net.IPAddress

-- for replicateM
-- import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Foldable ()
import Foreign.C.Types (CInt)
import Foreign.C.Error
-- import qualified System.Linux.Netlink as NL
import System.Linux.Netlink as NL
import System.Linux.Netlink.GeNetlink as GENL
import System.Linux.Netlink.Constants as NLC
import System.Linux.Netlink.Constants (eRTM_NEWADDR)
-- import System.Linux.Netlink.Helpers
import System.Log.FastLogger
import System.Linux.Netlink.GeNetlink.Control
import qualified System.Linux.Netlink.Simple as NLS
import qualified System.Linux.Netlink.Route as NLR

import System.Process
import System.Exit
import Data.Word (Word16, Word32)
-- import qualified Data.Bits as Bits -- (shiftL, )
-- import Data.Bits ((.|.))
import Data.Serialize.Get (runGet)
import Data.Serialize.Put
-- import Data.Either (fromRight)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Char8 (unpack, init)

-- import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map

import Data.Bits (Bits(..))

-- https://hackage.haskell.org/package/bytestring-conversion-0.1/candidate/docs/Data-ByteString-Conversion.html#t:ToByteString
-- contains ToByteString / FromByteString
-- import Data.ByteString.Conversion
import Debug.Trace
-- import Control.Exception

import Control.Concurrent
import Control.Exception (assert)
-- import Control.Concurrent.Chan
-- import Data.IORef
-- import Control.Concurrent.Async
import System.IO.Unsafe
import Data.Aeson

-- for getEnvDefault
import System.Environment.Blank()


-- for writeUTF8File
-- import Distribution.Simple.Utils
-- import Distribution.Utils.Generic

-- STM = State Thread Monad ST monad
-- import Data.IORef
-- MVar can be empty contrary to IORef !
-- globalMptcpSock :: IORef MptcpSocket

{-# NOINLINE globalMptcpSock #-}
globalMptcpSock :: MVar MptcpSocket
globalMptcpSock = unsafePerformIO newEmptyMVar

{-# NOINLINE globalMetricsSock  #-}
globalMetricsSock :: MVar NetlinkSocket
globalMetricsSock = unsafePerformIO newEmptyMVar

{-# NOINLINE globalInterfaces #-}
-- TODO Map search in a list
-- IP , Interface
-- globalInterfaces :: MVar (Map.Map Word32 NetworkInterface)
globalInterfaces :: MVar (AvailablePaths)
globalInterfaces = unsafePerformIO newEmptyMVar

iperfClientPort :: Word16
iperfClientPort = 5500

iperfServerPort :: Word16
iperfServerPort = 5201

monitoringRateMs :: Int
monitoringRateMs = 100

-- should be able to ignore based on regex
interfacesToIgnore :: [String]
interfacesToIgnore = [
  "virbr0"
  , "virbr1"
  , "nlmon0"
  , "ppp0"
  , "lo"
  ]

-- the path manager used in the 
pathManager :: PathManager
pathManager = ndiffports

data MyState = MyState {
  socket :: MptcpSocket -- ^Socket
  -- ThreadId/MVar
  , connections :: Map.Map MptcpToken (ThreadId, MVar MptcpConnection)
}
-- deriving Show



-- , dstIp = fromIPv4 localhost
authorizedCon1 :: TcpConnection
authorizedCon1 = TcpConnection {
        srcIp = fromIPv4 $ Net.IPv4.ipv4 192 168 0 128
        , dstIp = fromIPv4 $ Net.IPv4.ipv4 202 214 86 51
        , srcPort = iperfClientPort
        , dstPort = iperfServerPort
        , priority = Nothing
        , subflowInterface = Nothing
        , localId = 0
        , remoteId = 0
        , inetFamily = eAF_INET
    }

filteredConnections :: [TcpConnection]
filteredConnections = [
    authorizedCon1
    ]


-- inspired by CtrlPacket
  -- token :: MptcpToken
-- MptcpNewConnection
-- data MptcpPacket = MptcpPacket {
--       mptcpHeader     :: Header
--     , mptcpGeHeader   :: GenlHeader
--     , mptcpData   :: MptcpData

-- data MptcpData = MptcpData {
--     mptcpAttributes :: [MptcpAttr]
--   } deriving (Eq)

-- TODO are they generated
dumpCommand :: MptcpGenlEvent -> String
dumpCommand x = show x ++ " = " ++ show (fromEnum x)
-- dumpCommand MPTCP_CMD_UNSPEC  = " MPTCP_CMD_UNSPEC  0"
-- dumpCommand MPTCP_EVENT_SUB_ERROR = show MPTCP_EVENT_SUB_ERROR + show fromEnum MPTCP_EVENT_SUB_ERROR

dumpMptcpCommands :: MptcpGenlEvent -> String
dumpMptcpCommands MPTCP_CMD_EXIST = dumpCommand MPTCP_CMD_EXIST
dumpMptcpCommands x = dumpCommand x ++ "\n" ++ dumpMptcpCommands (succ x)


-- todo use it as a filter
data Sample = Sample {
  command    :: String
  , serverIP     :: IPv4
  -- , clientIP      :: IPv4
  , quiet      :: Bool
  , enthusiasm :: Int
  }


-- TODO use 3rd party library levels
-- data Verbosity = Normal | Debug


dumpSystemInterfaces :: IO()
dumpSystemInterfaces = do
  putStrLn "Dumping interfaces"
  -- isEmptyMVar globalInterfaces
  res <- tryReadMVar globalInterfaces 
  case (res) of
    Nothing -> putStrLn "No interfaces"
    Just interfaces -> putStrLn $ show interfaces

  putStrLn "End of dump"


-- TODO register subcommands instead
-- see https://stackoverflow.com/questions/31318643/optparse-applicative-how-to-handle-no-arguments-situation-in-arrow-syntax
--  ( command "add" (info addOptions ( progDesc "Add a file to the repository" ))
 -- <> command "commit" (info commitOptions ( progDesc "Record changes to the repository" ))
-- can use several constructors depending on the PM ?
-- eitherReader
-- ByteString
-- <>  ( metavar "ServerIP"
--readerError
sample :: Parser Sample
sample = Sample
      <$> argument str
          ( metavar "CMD"
         <> help "What to do" )
      <*> argument (eitherReader $ \x -> case (Net.IPv4.decodeString x) of
                                            Nothing -> Left "could not parse"
                                            Just ip -> Right ip)
        ( metavar "ServerIP"
         <> help "ServerIP to let through (e.g.: 202.214.86.51 )" )
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



-- inspired by makeNL80211Socket Create a 'NL80211Socket' this opens a genetlink
-- socket and gets the family id
-- TODO should record the token too
-- TODO should join the group !!
makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket = do
    -- for legacy reasons this opens a route socket
  sock <- GENL.makeSocket
  res <- getFamilyIdS sock mptcpGenlName
  case res of
    Nothing -> error $ "Could not find family " ++ mptcpGenlName
    Just fid -> return  (MptcpSocket sock (trace ("family id"++ show fid ) fid))


-- Used tuples
-- sock <- makeSocket
makeMetricsSocket :: IO NetlinkSocket
makeMetricsSocket = do
    -- NETLINK_SOCK_DIAG == NETLINK_INET_DIAG
  sock <- makeSocketGeneric eNETLINK_SOCK_DIAG
  return sock

-- used in tests
startMonitorExternalProcess :: MptcpToken -> CreateProcess
startMonitorExternalProcess token =
  let
    params = proc "daemon" [ show token ]
    -- todo pass the token via the environment
    newParams = params {
        -- cmdspec = RawCommand "monitor" [ show token ]
        new_session = True
        -- cwd
        -- env
    }
  in
    -- return "toto"
    newParams

-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs :: Int -> IO()
sleepMs n = threadDelay (n * 1000)

-- Need a specific chan
-- TODO it should start a forkIO that fetches metrics and updates stuff
-- Chan ->
-- readChan is blocking sadly
-- modifyMVar_


-- here we may want to run mptcpnumerics to get some results
updateSubflowMetrics :: TcpConnection -> IO ()
updateSubflowMetrics subflow = do
    putStrLn "Reading metrics sock Mvar..."
    sockMetrics <- readMVar globalMetricsSock

    putStrLn "Reading mptcp sock Mvar..."
    mptcpSock <- readMVar globalMptcpSock
    putStrLn "Finished reading"
    let queryPkt = genQueryPacket (Right subflow) [TcpListen, TcpEstablished]
         [InetDiagCong, InetDiagInfo, InetDiagMeminfo]
    -- let (MptcpSocket testSock familyId ) = mptcpSock
    sendPacket sockMetrics queryPkt
    putStrLn "Sent the TCP SS request"

    -- exported from my own version !!
    recvMulti sockMetrics >>= inspectIdiagAnswers
    -- for now let's discard the answer
    -- recvOne sockMetrics >>= inspectIdiagAnswers

    -- let capCwndPkt = genCapCwnd familyId
    -- sendPacket testSock capCwndPkt >> putStrLn "Sent the Cap command"

-- TODO
-- clude child configs in grub menu #45345
-- send MPTCP_CMD_SND_CLAMP_WINDOW
-- TODO we need the token to generate the command ?
--token, family, loc_id, rem_id, [saddr4 | saddr6,
-- daddr4 | daddr6, dport [, sport, backup, if_idx]]

genCapCwnd :: MptcpToken
              -> Word16 -- ^family id
              -> MptcpPacket
genCapCwnd token familyId =
    assert (hasFamily attrs) pkt
    where
        pkt = genMptcpRequest familyId MPTCP_CMD_SND_CLAMP_WINDOW True attrs
        -- attrs = Map.empty
        attrs = [
            MptcpAttrToken token
            , SubflowFamily eAF_INET
            , LocalLocatorId 0
            -- TODO check emote locator ?
            , RemoteLocatorId 0
            -- TODO fix
            , SubflowInterface $ getInterfaceIdFromIP (fromIPv4 localhost)
            ]
    -- putStrLn "while waiting for a real implementation"

-- Use a Mvar here to
startMonitorConnection :: MptcpSocket -> MVar MptcpConnection -> IO ()
startMonitorConnection mptcpSock mConn = do
    -- ++ show token
    let (MptcpSocket sock familyId) = mptcpSock
    putStr "Start monitoring connection..."
    -- as long as conn is not empty we keep going ?
    -- for this connection
    -- query metrics for the whole MPTCP connection
    con <- readMVar mConn
    putStrLn $ show con ++ "..."
    let token = connectionToken con


    let resetAttrs = [ (MptcpAttrToken token ), (LocalLocatorId 0)]
    let masterSf = head $ subflows con
    let newSubflowAttrs = [
            MptcpAttrToken token
            ] ++ (subflowAttrs $ masterSf { srcPort = 0 })
    let capSubflowAttrs = [
            MptcpAttrToken token
            , SubflowMaxCwnd 3
            , SubflowSourcePort $ srcPort masterSf
            ] ++ (subflowAttrs masterSf)
    let resetPkt = resetConnectionPkt mptcpSock resetAttrs
    let newSfPkt = newSubflowPkt mptcpSock newSubflowAttrs
    -- let capCwndPkt = capCwndPkt mptcpSock capSubflowAttrs

    -- updateCwndCap
    -- queryOne
    -- putStrLn $ "Sending RESET for token " ++ show token
    -- query sock resetPkt >>= inspectAnswers

    -- putStrLn $ "Master sf " ++ show masterSf
    -- putStrLn $ "Trying to cap subflow cwd... with token " ++ show token
    -- putStrLn $ "Sending " ++ show capCwndPkt

    -- putStrLn $ "Master sf " ++ show masterSf
    -- putStrLn $ "Trying to create new subflow... with token " ++ show token
    -- putStrLn $ "Sending " ++ show newSfPkt
    -- query sock newSfPkt >>= inspectAnswers

    putStrLn "Running mptcpnumerics"
    cwnds <- getCapsForConnection con

    putStrLn $ "Requesting to set cwnds..." ++ show cwnds
    -- TODO capCwndAttrs capCwndPkt
    -- KISS for now (capCwndPkt mptcpSock )
    -- zip caps (subflows con)
    let attrsList = map (\(cwnd, sf) -> capCwndAttrs token sf cwnd ) (zip cwnds (subflows con))
    -- >> map (capCwndPkt mptcpSock ) >>= putStrLn "toto"

    -- query sock capCwndPkt >>= inspectAnswers
    -- de type [MptcpPacket]
    let cwndPackets = map (capCwndPkt mptcpSock) attrsList
    mapM_ (query sock) cwndPackets >> putStrLn "test"
         -- >>= inspectAnswers

    -- then we should send a request for each cwnd
    mapM_ updateSubflowMetrics (subflows con)

    sleepMs monitoringRateMs
    putStrLn "Finished monitoring token "
    -- call ourself again
    startMonitorConnection mptcpSock mConn


-- | This should return a list of cwnd to respect a certain scenario
-- invoke
-- 1. save the connection to a JSON file and pass it to mptcpnumerics
-- 2.
-- 3.
-- Maybe ?
getCapsForConnection :: MptcpConnection -> IO [Word32]
getCapsForConnection con = do
    -- returns a bytestring
    let bs = Data.Aeson.encode con
    let subflowCount = length $ subflows con
    let tmpdir = "/tmp"
    let filename = tmpdir ++ "/" ++ "mptcp_" ++ (show $ subflowCount)  ++ "_" ++ (show $ connectionToken con) ++ ".json"
    -- let tmpdir = getEnvDefault "TMPDIR" "/tmp"

    -- encode token in filename
    -- fromMaybe $
    Data.ByteString.Lazy.writeFile filename bs

    -- eitherDecode
    -- TODO run readProcessWithExitCode instead
    -- FilePath -> [String] -> String -> IO (ExitCode, String, String)

    -- TODO to keep it simple it should return a list of CWNDs to apply
    -- readProcessWithExitCode  binary / args / stdin
    (exitCode, stdout, stderr) <- readProcessWithExitCode "./hs/fake_solver" [filename, show subflowCount] ""
    case exitCode of
        -- for now simple, we might read json afterwards
        ExitSuccess -> return (read stdout :: [Word32])
        ExitFailure val -> error $ "stdout:" ++ stdout ++ " stderr: " ++ stderr

-- type Attributes = Map Int ByteString
-- the library contains showAttrs / showNLAttrs
showAttributes :: Attributes -> String
showAttributes attrs =
  let
    mapped = Map.foldrWithKey (\k v -> (dumpAttribute k v ++)  ) "\n " attrs
  in
    mapped

putW32 :: Word32 -> ByteString
putW32 x = runPut (putWord32host x)

-- removeSubflow :: MptcpSocket -> MptcpToken -> LocId -> IO [GenlPacket NoData]
-- removeSubflow (MptcpSocket socket fid) token locId = let

-- I want to override the GenlHeader version
newtype GenlHeaderMptcp = GenlHeaderMptcp GenlHeader
instance Show GenlHeaderMptcp where
  show (GenlHeaderMptcp (GenlHeader cmd ver)) =
    "Header: Cmd = " ++ show cmd ++ ", Version: " ++ show ver ++ "\n"

inspectAnswers :: [GenlPacket NoData] -> IO ()
inspectAnswers packets = do
  mapM_ inspectAnswer packets
  putStrLn "Finished inspecting answers"

-- showPacketCustom :: GenlPacket NoData -> String
-- showPacketCustom pkt = let
--   hdr = (genlDataHeader pkt )
--   in showPacket pkt

showHeaderCustom :: GenlHeader -> String
showHeaderCustom hdr = show hdr

inspectAnswer :: GenlPacket NoData -> IO ()
-- inspectAnswer packet = putStrLn $ "Inspecting answer:\n" ++ showPacket packet
-- (GenlData NoData)
-- inspectAnswer (Packet hdr (GenlData ghdr NoData) attributes) = putStrLn $ "Inspecting answer:\n"
inspectAnswer (Packet _ (GenlData hdr NoData) attributes) = let
    cmd = genlCmd hdr
  in
    putStrLn $ "Inspecting answer custom:\n" ++ showHeaderCustom hdr
            ++ "Supposing it's a mptcp command: " ++ dumpCommand ( toEnum $ fromIntegral cmd)

inspectAnswer pkt = putStrLn $ "Inspecting answer:\n" ++ showPacket pkt



-- should have this running in parallel
queryAddrs :: NLR.RoutePacket
queryAddrs = NL.Packet
    (NL.Header NLC.eRTM_GETADDR (NLC.fNLM_F_ROOT .|. NLC.fNLM_F_MATCH .|. NLC.fNLM_F_REQUEST) 0 0)
    (NLR.NAddrMsg 0 0 0 0 0)
    mempty


-- TODO show host interfaces
-- dumpInterfaces

-- TODO we should store this
-- really only interested in NAddrMsg
-- handleMessage :: NLR.Message -> IO ()
-- handleMessage (NLR.NLinkMsg _ _ _ ) = putStrLn "Ignoring NLinkMsg"
-- handleMessage (NLR.NNeighMsg _ _ _ _ _ ) = putStrLn $ "Ignoring NNeighMsg"
-- lol here we need to update the list of interfaces
-- handleMessage (NLR.NAddrMsg family maskLen flags scope addrIntf) = do
--   infs <- takeMVar globalInterfaces
--   -- TODO update w
--   -- newInfs = infs

--   -- decode/getIFAddr
--   let newInf = NetworkInterface (IP ) addrIntf
--   let newInfs = Map.insert addrIntf infs
--   putMVar globalInterfaces newInfs



-- TODO we should use the 
handleInterfaceNotification
  :: AddressFamily -> Attributes -> Word32 -> Maybe NetworkInterface
handleInterfaceNotification addrFamily attrs addrIntf =

  -- case of
  --   Nothing -> Nothing
  --   Just val -> 
  -- TODO
  -- filter on flags too (UP), should be != LOOPBACK
  -- lo: <LOOPBACK,UP,LOWER_UP> and
  -- eno1: <BROADCAST,MULTICAST,UP,LOWER_UP
  case ifNameM of
    Nothing -> Nothing
    Just ifName -> case (elem ifName interfacesToIgnore ) of
                        True -> Nothing
                        False -> Just $ NetworkInterface ip ifName addrIntf
  where
    -- ip = undefined
    -- gets the bytestring / assume it always work
  ipBstr = fromMaybe empty (NLR.getIFAddr attrs)
  ifNameBstr = (Map.lookup NLC.eIFLA_IFNAME attrs)
  ifNameM = getString <$> ifNameBstr
  -- ip = getIPFromByteString addrFamily ipBstr
  ip = case (getIPFromByteString addrFamily ipBstr) of
    Right val -> val
    Left err -> undefined

-- taken from netlink
getString :: ByteString -> String
getString b = unpack (init b)

-- TODO handle remove/new event
handleAddr :: Either String NLR.RoutePacket -> IO ()
handleAddr (Left errStr) = putStrLn $ "Error decoding packet: " ++ errStr
handleAddr (Right (DoneMsg hdr)) =
  putStrLn $ "Error decoding packet: " ++ show hdr
handleAddr (Right (ErrorMsg hdr errorInt errorBstr)) =
  putStrLn $ "Error decoding packet: " ++ show hdr
-- TODO need handleMessage pkt
-- family maskLen flags scope addrIntf
handleAddr (Right (Packet hdr pkt attrs)) = do
  (putStrLn $ "received packet" ++ show pkt)
  oldIntfs <- trace "taking MVAR" (takeMVar globalInterfaces)

  let toto = (case pkt of
        arg@NLR.NAddrMsg{} ->
          let resIntf = handleInterfaceNotification (NLR.addrFamily arg) attrs (NLR.addrInterfaceIndex arg)
          in case resIntf of 
                Nothing -> oldIntfs
                Just newIntf -> let
                  ip = ipAddress newIntf
                  in if msgType == eRTM_NEWADDR
                        then trace "adding ip" (Map.insert ip newIntf oldIntfs)
                        -- >> putStrLn "Added interface"
                        else if msgType == eRTM_GETADDR
                        then trace "GET_ADDR" oldIntfs

                        else if msgType == eRTM_DELADDR
                        then
                        trace "deleting ip" (Map.delete (ip) oldIntfs)
                        -- >> putStrLn "Removed interface"
                        else trace "other type" oldIntfs

        -- _ -> error "can't be anything else"
        arg@NLR.NNeighMsg{} -> trace "neighbor msg" oldIntfs
        arg@NLR.NLinkMsg{} -> trace "link msg" oldIntfs
        )
  -- case toto of
  --     Nothing -> oldIntfs
      -- Just val ->
  trace ("putting mvar") (putMVar globalInterfaces $! (toto))

 where
    -- gets the bytestring
    msgType = messageType hdr

-- (arg@DiagTcpInfo{})


onNewConnection :: MptcpSocket -> Attributes -> IO ()
onNewConnection sock attributes = do
    -- TODO use getAttribute instead
    let token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes
    let locId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_LOC_ID) attributes
    let answer = "e"
    -- answer <-  Prelude.getLine
    -- putStrLn "What do you want to do ? (c.reate subflow, d.elete connection, r.eset connection)"
    -- announceSubflow sock token >>= inspectAnswers >> putStrLn "Finished announcing"
    case answer of
      "c" -> do
        putStrLn "Creating new subflow !!"
        -- let pkt = createNewSubflow sock token attributes
        return ()
      "d" -> putStrLn "Not implemented"
        -- removeSubflow sock token locId >>= inspectAnswers >> putStrLn "Finished announcing"
      -- "e" -> putStrLn "check for existence" >>
      --     checkIfSocketExists sock token >>= inspectAnswers
      "r" -> putStrLn "Reset the connection" >>
        -- TODO expects token
        -- TODO discard result
          -- resetTheConnection sock token >>= inspectAnswers >>
          putStrLn "Finished resetting"
      _ -> onNewConnection sock attributes
    return ()


-- unknownConnectionEvent :: M
-- unknownConnectionMonitor :: MptcpToken -> MptcpPacket
-- pass [MptcpAttribute] instead ?
-- TODO here we should pass a path manager
dispatchPacketForKnownConnection :: MptcpConnection
                                    -> MptcpGenlEvent
                                    -> Attributes
                                    -> MptcpConnection
                                    -- -> IO MyState
dispatchPacketForKnownConnection con event attributes = let
        token = connectionToken con
    in
    case event of
      -- TODO wait for established instead ?
      -- MPTCP_EVENT_CREATED -> do

      MPTCP_EVENT_ESTABLISHED -> con
        -- do
        -- putStrLn "Connection established !"
        -- TODO llok
        -- return oldState

      MPTCP_EVENT_ANNOUNCED -> con

          -- what if it's local
            -- case makeAttributeFromMaybe MPTCP_ATTR_REM_ID attrs of
            --     Nothing -> con
            --     Just localId -> putStrLn "Found a match"
            --     -- swapMVar / withMVar / modifyMVar
            --     mptcpConn <- takeMVar mConn
            --     -- TODO we should trigger an update in the CWND
            --     let newCon = mptcpConnAddLocalId mptcpConn
            --     newCon
                -- let newState = oldState
        -- return oldState

      MPTCP_EVENT_CLOSED ->
        -- putStrLn $ "Connection closed, deleting token " ++ show token
        -- let newState = oldState { connections = Map.delete token (connections oldState) }
        -- TODO we should kill the thread with killThread or empty the mVar !!
        -- let newState = oldState
        -- putStrLn $ "New state"
        -- return newState
        error "should never be called"

      MPTCP_EVENT_SUB_ESTABLISHED -> let
            subflow = subflowFromAttributes attributes
            newCon = mptcpConnAddSubflow con subflow

            in
                newCon
        -- let newState = oldState
        -- putMVar con newCon
        -- let newState = oldState { connections = Map.insert token newCon (connections oldState) }
        -- TODO we should insert the
        -- newConn <-
        -- return newState

      -- TODO remove
      MPTCP_EVENT_SUB_CLOSED -> con
            -- putStrLn "Subflow closed"
            -- putStrLn "TODO remove subflow"
            -- >> mptcpConnRemoveSubflow
            -- return oldState

      MPTCP_CMD_EXIST -> con
      _ -> error $ "should not happen " ++ show event
      -- >> return oldState

-- TODO pass a PathManager
-- |^
--
dispatchPacket :: MyState -> MptcpPacket -> IO MyState
dispatchPacket oldState (Packet hdr (GenlData genlHeader NoData) attributes) = let
        cmd = toEnum $ fromIntegral $ genlCmd genlHeader
        (MyState mptcpSock conns) = oldState

        -- i suppose token is always available right ?
        token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes
        maybeMatch = Map.lookup token (connections oldState)
    in
        case maybeMatch of
            -- Unknown token
            Nothing -> case cmd of
                MPTCP_EVENT_CREATED -> do
                    putStrLn "Ignoring Creating EVENT"
                    return oldState

                MPTCP_EVENT_ESTABLISHED -> let subflow = subflowFromAttributes attributes
                    in
                    if subflow `notElem` filteredConnections
                        then do
                            putStrLn $ "filtered out connection" ++ show subflow
                            putStrLn $ "it was compared with " ++ show authorizedCon1
                            return oldState
                        else (do
                                let newMptcpConn = MptcpConnection token [ subflow ] [] []
                                newConn <- newMVar newMptcpConn
                                putStrLn $ "Connection established !!\n" ++ showAttributes attributes
                                -- onNewConnection mptcpSock attributes

                                -- open additionnal subflows
                                (onMasterEstablishement pm) newConn

                                -- start monitoring connection
                                handle <- forkOS (startMonitorConnection mptcpSock newConn)
                                -- r <- createProces $ startMonitor token
                                -- putStrLn $ "Connection created !!\n" ++ show subflow
                                let newState = oldState { connections = Map.insert token (handle, newConn) (connections oldState) }
                                return newState)

                _ -> return oldState

            Just (threadId, mvarConn) -> case cmd of

                MPTCP_EVENT_CREATED -> error "We should not receive MPTCP_EVENT_CREATED from here !!!"
                MPTCP_EVENT_SUB_CLOSED -> do
                    putStrLn $ "SUBFLOW WAS CLOSED"
                    return oldState

                MPTCP_EVENT_CLOSED -> do
                    putStrLn $ "Killing thread " ++ show threadId
                    killThread threadId
                    -- TODO remove
                    return $ oldState { connections = Map.delete token (connections oldState) }
                    -- let newState = oldState

            -- TODO update connection
                -- TODO filter first
                _ -> do
                    mptcpConn <- takeMVar mvarConn
                    let newConn = dispatchPacketForKnownConnection mptcpConn cmd attributes
                    putMVar mvarConn newConn
                    -- TODO update state
                    let newState = oldState {
                        connections = Map.insert token (threadId, mvarConn) (connections oldState)
                    }
                    return newState


dispatchPacket s (DoneMsg err) =
  putStrLn "Done msg" >> return s


-- EOK shouldn't be an ErrorMsg when it receives EOK ?
dispatchPacket s (ErrorMsg hdr errCode errPacket) = do
  putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n" ++ show errPacket
  return s

-- ++ show errPacket
showError :: Show a => Packet a -> IO ()
showError (ErrorMsg hdr errCode errPacket) = do
  putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n"
showError _ = error "Not the good overload"

-- netlink must contain sthg for it
-- /usr/include/asm/errno.h
showErrCode :: CInt -> String
showErrCode err
  | Errno err == ePERM = "EPERM"
  | Errno err == eOK = "EOK"
  | otherwise = show err

-- showErrCode err = case err of
-- -- show err
--   ePERM -> "EPERM"
--   eNOTCONN -> "NOT connected"

inspectResult :: MyState -> Either String MptcpPacket -> IO()
inspectResult myState result =  case result of
    Left ex -> putStrLn $ "An error in parsing happened" ++ show ex
    Right myPack -> dispatchPacket myState myPack >> putStrLn "Valid packet"

doDumpLoop :: MyState -> IO MyState
doDumpLoop myState = do
    let (MptcpSocket simpleSock fid) = socket myState

    results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]

    -- TODO retrieve packets
    mapM_ (inspectResult myState) results

    newState <- doDumpLoop myState
    return newState

-- regarder dans query/joinMulticastGroup/recvOne
listenToEvents :: MptcpSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents (MptcpSocket sock fid) my_group = do
  -- joinMulticastGroup  returns IO ()
  -- TODO should check it works correctly !
  joinMulticastGroup sock (grpId my_group)
  putStrLn $ "Joined grp " ++ grpName my_group
  let globalState = MyState mptcpSocket  Map.empty
  _ <- doDumpLoop globalState
  putStrLn "TOTO"
  where
    mptcpSocket = MptcpSocket sock fid
    -- globalState = MyState mptcpSocket Map.empty


-- testing
listenToMetricEvents :: NetlinkSocket -> CtrlAttrMcastGroup  -> IO ()
listenToMetricEvents sock myGroup = do
  putStrLn "listening to metric events"
  joinMulticastGroup sock (grpId myGroup)
  putStrLn $ "Joined grp " ++ grpName myGroup
  -- _ <- doDumpLoop globalState
  -- putStrLn "TOTO"
  -- where
  --   -- mptcpSocket = MptcpSocket sock fid
  --   globalState = MyState mptcpSocket Map.empty


createLogger :: IO LoggerSet
createLogger = newStdoutLoggerSet defaultBufSize


-- for big endian
-- https://mail.haskell.org/pipermail/beginners/2010-October/005571.html
-- foldl' :: (b -> a -> b) -> b -> Maybe a -> b
-- fromOctetsBE :: [Word8] -> Word32
-- fromOctetsBE = foldl' accum 0
--   where
--     accum a o = (a `Bits.shiftL` 8) .|. fromIntegral o
--     -- maybe I could use putIArrayOf instead

-- fromOctetsLE :: [Word8] -> Word32
-- fromOctetsLE = fromOctetsBE . reverse



dumpExtensionAttribute :: Int -> ByteString -> String
dumpExtensionAttribute attrId value = let
        eExtId = (toEnum attrId :: IDiagExt)
        ext_m = loadExtension attrId value
    in
        case ext_m of
            Nothing -> "Could not load " ++ show eExtId ++ " \n"
            Just ext -> traceId (show eExtId) ++ " " ++ showExtension ext ++ " \n"
    -- show $ (toEnum attrId :: IDiagExt)

showExtensionAttributes :: Attributes -> String
showExtensionAttributes attrs =
    let
        -- $ loadExtension
        mapped = Map.foldrWithKey (\k v -> (dumpExtensionAttribute k v ++ )) "Dumping extensions:\n " attrs
    in
        mapped


--
inspectIDiagAnswer :: Packet InetDiagMsg -> IO ()
inspectIDiagAnswer (Packet hdr cus attrs) =
    putStrLn ("Idiag custom " ++ show cus) >>
    putStrLn ("Idiag header " ++ show hdr) >>
    putStrLn (showExtensionAttributes attrs)
inspectIDiagAnswer p = putStrLn $ "test" ++ showPacket p

-- inspectIDiagAnswer (DoneMsg err) = putStrLn "DONE MSG"
-- (GenlData NoData)
-- inspectIdiagAnswer (Packet hdr (GenlData ghdr NoData) attributes) = putStrLn $ "Inspecting answer:\n"
-- inspectIdiagAnswer (Packet _ (GenlData hdr NoData) attributes) = let
--     cmd = genlCmd hdr
--   in
--     putStrLn $ "Inspecting answer custom:\n" ++ showHeaderCustom hdr
--             ++ "Supposing it's a mptcp command: " ++ dumpCommand ( toEnum $ fromIntegral cmd)



-- Updates the list of interfaces
-- should run in background
--
trackSystemInterfaces :: IO()
trackSystemInterfaces = do
  -- check routing information
  routingSock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
  let cb = NLS.NLCallback (pure ()) (handleAddr . runGet getGenPacket)
  NLS.nlPostMessage routingSock queryAddrs cb
  NLS.nlWaitCurrent routingSock


-- la en fait c des reponses que j'obtiens ?
inspectIdiagAnswers :: [Packet InetDiagMsg] -> IO ()
inspectIdiagAnswers packets = do
  putStrLn "Start inspecting IDIAG answers"
  mapM_ inspectIDiagAnswer packets
  putStrLn "Finished inspecting answers"

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  options <- execParser opts

  -- crashes when threaded
  -- logger <- createLogger
  -- pushLogStr logger (toLogStr "ok")

  -- globalState <- newEmptyMVar
  -- putStrLn $ "RESET" ++ show MPTCP_CMD_REMOVE
  -- putStrLn $ dumpMptcpCommands MPTCP_CMD_UNSPEC
  putStrLn "Creating MPTCP netlink socket..."

  -- add the socket too to an MVar ?
  (MptcpSocket sock  fid) <- makeMptcpSocket
  -- putMVar instead !
  -- globalState <- newMVar $ MyState (MptcpSocket sock  fid) Map.empty
  putMVar globalMptcpSock (MptcpSocket sock  fid)

  putStrLn "Creating metrics netlink socket..."
  sockMetrics <- makeMetricsSocket
  -- putMVar
  putMVar globalMetricsSock sockMetrics

  -- a threadid
  -- TODO put an empty map
  putStrLn "Now Tracking system interfaces..."
  putMVar globalInterfaces Map.empty
  routeNl <- forkIO trackSystemInterfaces

  dumpSystemInterfaces 
  -- check routing information
  -- routingSock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
  -- let cb = NLS.NLCallback (pure ()) (handleAddr . runGet getGenPacket)
  -- NLS.nlPostMessage routingSock queryAddrs cb
  -- NLS.nlWaitCurrent routingSock

  return ()
  putStr "socket created. MPTCP Family id " >> Prelude.print fid
  -- putStr "socket created. tcp_metrics Family id " >> print fidMetrics
  -- That's what I should use in fact !! (Word16, [CtrlAttrMcastGroup])
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  -- Each netlink family has a set of 32 multicast groups
  -- mcastMetricGroups <- getMulticastGroups sockMetrics fidMetrics
  mcastMptcpGroups <- getMulticastGroups sock fid
  mapM_ Prelude.print mcastMptcpGroups

  -- mapM_ (listenToMetricEvents sockMetrics) mcastMetricGroups
  mapM_ (listenToEvents (MptcpSocket sock fid)) mcastMptcpGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastMptcpGroups )
  putStrLn "finished"

