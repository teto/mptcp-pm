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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (concat, init)
import Options.Applicative hiding (value, ErrorMsg, empty)
import qualified Options.Applicative (value)

-- For TcpState, FFI generated
import Net.SockDiag
import Net.Mptcp
--  hiding(mapIPtoInterfaceIdx)
import Net.Mptcp.PathManager
import Net.Mptcp.PathManager.Default
import Net.Tcp
import Net.IP
import Net.IPv4 hiding (print)
-- import Net.IPAddress

import Net.SockDiag.Constants
import Net.Tcp.Constants
import Net.Mptcp.Constants


-- for readList
import Text.Read
import Data.Text (pack)

-- for replicateM
import Control.Monad (foldM)
-- fromMaybe, 
import Data.Maybe (catMaybes)
-- import Data.Foldable (concat)
import Foreign.C.Types (CInt)
-- for eOK, ePERM
import Foreign.C.Error
-- import qualified System.Linux.Netlink as NL
import System.Linux.Netlink as NL
import System.Linux.Netlink.GeNetlink as GENL
import System.Linux.Netlink.Constants as NLC
-- import System.Linux.Netlink.Constants (eRTM_NEWADDR)
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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (writeFile)
-- import Data.ByteString.Char8 (unpack, init)

-- import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text
import Data.Bits (Bits(..))

-- https://hackage.haskell.org/package/bytestring-conversion-0.1/candidate/docs/Data-ByteString-Conversion.html#t:ToByteString
-- contains ToByteString / FromByteString
-- import Data.ByteString.Conversion
import Debug.Trace
-- import Control.Exception

import Control.Concurrent
-- import Control.Exception (assert)
-- import Control.Concurrent.Chan
-- import Data.IORef
-- import Control.Concurrent.Async
import System.IO.Unsafe
import System.IO (stderr, Handle)
import Data.Aeson
-- to merge MptcpConnection export and Metrics
import Data.Aeson.Extra.Merge  (lodashMerge)

-- import GHC.Generics

-- for getEnvDefault
import System.Environment.Blank()

-- trying hslogger
import System.Log.Logger (rootLoggerName, infoM, debugM, Priority(DEBUG), Priority(INFO), setLevel, updateGlobalLogger)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
-- for writeUTF8File
-- import Distribution.Simple.Utils
-- import Distribution.Utils.Generic
-- import System.Log.Handler (setFormatter)

-- STM = State Thread Monad ST monad
-- import Data.IORef
-- MVar can be empty contrary to IORef !
-- globalMptcpSock :: IORef MptcpSocket
-- from unordered containers
-- import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM

{-# NOINLINE globalMptcpSock #-}
globalMptcpSock :: MVar MptcpSocket
globalMptcpSock = unsafePerformIO newEmptyMVar
-- du coup cette socket n'aurait meme plus besoin d'etre globale en fait ?

-- TODO remove
-- globalMetricsSock :: MVar NetlinkSocket
-- globalMetricsSock = unsafePerformIO newEmptyMVar

-- {-# NOINLINE globalInterfaces #-}
-- globalInterfaces :: MVar AvailablePaths
-- globalInterfaces = unsafePerformIO newEmptyMVar

iperfClientPort :: Word16
iperfClientPort = 5500

iperfServerPort :: Word16
iperfServerPort = 5201

-- |
onSuccessSleepingDelay :: Int
onSuccessSleepingDelay = 300


-- When it couldn't set the correct value
onFailureSleepingDelay :: Int
onFailureSleepingDelay = 100

-- should be able to ignore based on regex
-- TODO see interfacesToIgnore 
-- interfacesToIgnore :: [String]
-- interfacesToIgnore = [
--   "virbr0"
--   , "virbr1"
--   , "nlmon0"
--   , "ppp0"
--   , "lo"
--   ]

-- |the path manager used in the 
pathManager :: PathManager
pathManager = meshPathManager

-- |Helper to pass information across functions
data MyState = MyState {
  socket :: MptcpSocket -- ^Socket
  -- ThreadId/MVar
  , connections :: Map.Map MptcpToken (ThreadId, MVar MptcpConnection)
}
-- deriving Show

-- https://stackoverflow.com/questions/51407547/how-to-update-a-field-of-a-json-object
addJsonKey :: Data.Text.Text -> Value -> Value -> Value
addJsonKey key val (Object xs) = Object $ HM.insert key val xs
addJsonKey _ _ xs = xs

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
    }

authorizedCon2 :: TcpConnection
authorizedCon2 = authorizedCon1 { srcIp = fromIPv4 $ Net.IPv4.ipv4 192 168 0 130 }

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

loggerName :: String
loggerName = "main"

dumpSystemInterfaces :: IO()
dumpSystemInterfaces = do
  putStrLn "Dumping interfaces"
  -- isEmptyMVar globalInterfaces
  res <- tryReadMVar globalInterfaces 
  case res of
    Nothing -> putStrLn "No interfaces"
    Just interfaces -> Prelude.print interfaces

  putStrLn "End of dump"


-- TODO register subcommands instead
-- see https://stackoverflow.com/questions/31318643/optparse-applicative-how-to-handle-no-arguments-situation-in-arrow-syntax
--  ( command "add" (info addOptions ( progDesc "Add a file to the repository" ))
 -- <> command "commit" (info commitOptions ( progDesc "Record changes to the repository" ))
-- can use several constructors depending on the PM ?
-- ByteString
sample :: Parser Sample
sample = Sample
      <$> argument str
          ( metavar "CMD"
         <> help "What to do" )
      -- TODO should accept hostname etc
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

-- | here we may want to run mptcpnumerics to get some results
updateSubflowMetrics :: NetlinkSocket -> TcpConnection -> IO SockDiagMetrics
updateSubflowMetrics sockMetrics subflow = do
    putStrLn "Updating subflow metrics"
    -- putStrLn "Reading mptcp sock Mvar..."
    -- mptcpSock <- readMVar globalMptcpSock
    -- putStrLn "Finished reading"
    let queryPkt = genQueryPacket (Right subflow) [TcpListen, TcpEstablished]
         [InetDiagCong, InetDiagInfo, InetDiagMeminfo]
    sendPacket sockMetrics queryPkt
    putStrLn "Sent the TCP SS request"

    -- exported from my own version !!
    -- TODO display number of answers
    --  SockDiagMetrics 
    putStrLn "Starting inspecting answers"
    answers <- recvMulti sockMetrics 
    let metrics_m = inspectIdiagAnswers answers
    -- >>= inspectIdiagAnswers >>= return
    -- return only the first ?
    -- filter ? keep only valud ones ?
    -- 
    return $ head (catMaybes metrics_m)
    -- putStrLn "Finished inspecting answers"


-- TODO
-- clude child configs in grub menu #45345
-- send MPTCP_CMD_SND_CLAMP_WINDOW
-- TODO we need the token to generate the command ?
--token, family, loc_id, rem_id, [saddr4 | saddr6,
-- daddr4 | daddr6, dport [, sport, backup, if_idx]]


{- |
  Starts monitoring a specific MPTCP connection
-}
-- Use a Mvar here to
startMonitorConnection :: MptcpSocket -> NetlinkSocket -> MVar MptcpConnection -> IO ()
startMonitorConnection mptcpSock sockMetrics mConn = do
    -- ++ show token
    let (MptcpSocket sock familyId) = mptcpSock
    myId <- myThreadId
    putStr $ show myId ++ "Start monitoring connection..."
    -- as long as conn is not empty we keep going ?
    -- for this connection
    -- query metrics for the whole MPTCP connection
    con <- readMVar mConn
    putStrLn "Showing MPTCP connection"
    putStrLn $ show con ++ "..."
    let token = connectionToken con

    -- TODO this is the issue
    -- not sure it's the master with a set
    let masterSf = Set.elemAt 0 (subflows con)

    -- Get updated metrics
    -- lastMetrics 
    -- let listOfIOSockMetrics = mapM (updateSubflowMetrics sockMetrics) (Set.toList $ subflows con)
    lastMetrics <- mapM (updateSubflowMetrics sockMetrics) (Set.toList $ subflows con)
    -- lastMetrics <- Control.Monad.sequence listOfIOSockMetrics

    putStrLn "Running mptcpnumerics"

    -- Then refresh the cwnd objective
    --MptcpConnectionWithMetrics 
    cwnds_m <- getCapsForConnection con lastMetrics
    case cwnds_m of
        Nothing -> do
            putStrLn "Couldn't fetch the values"
            sleepMs onFailureSleepingDelay
        Just cwnds -> do

            putStrLn $ "Requesting to set cwnds..." ++ show cwnds
            -- TODO fix
            -- KISS for now (capCwndPkt mptcpSock )
            let cwndPackets  = map (\(cwnd, sf) -> capCwndPkt mptcpSock con cwnd sf) (zip cwnds (Set.toList $ subflows con))

            mapM_ (sendPacket sock) cwndPackets

            sleepMs onSuccessSleepingDelay
    putStrLn "Finished monitoring token "

    -- call ourself again
    startMonitorConnection mptcpSock sockMetrics mConn



-- Hack to quickly be able to encode results with aeson
-- data MptcpConnectionWithMetrics = MptcpConnectionWithMetrics MptcpConnection [SockDiagMetrics] deriving (Generic)

-- instance FromJSON MptcpConnectionWithMetrics

-- instance ToJSON SubflowWithMetrics where
-- instance ToJSON (MptcpConnection, [SockDiagMetrics])  where
  -- toJSON MptcpConnectionWithMetrics mptcpConn metrics = object
  --   [ "name" .= toJSON (show $ connectionToken mptcpConn)
  --   , "sender" .= object [
  --         -- TODO here we could read from sysctl ? or use another SockDiagExtension
  --         "snd_buffer" .= toJSON (40 :: Int)
  --         , "capabilities" .= object []
  --       ]
  --   , "capabilities" .= object ([])
  --   -- TODO export with metrics
  --   , "subflows" .= object ([])
  --   ]


{-
  | This should return a list of cwnd to respect a certain scenario
 1. save the connection to a JSON file and pass it to mptcpnumerics
-}
getCapsForConnection :: MptcpConnection -> [SockDiagMetrics] -> IO (Maybe [Word32])
getCapsForConnection mptcpConn metrics = do
    -- returns a bytestring
    -- let (MptcpConnectionWithMetrics  mptcpConn metrics) = conWithMetrics
    let jsonConn = (toJSON mptcpConn)
    let merged = lodashMerge jsonConn (object [ "subflows" .= metrics ])
  -- (toJSON jsonConn) ++ (toJSON metrics)

    let bs = Data.Aeson.encode merged
    -- let bs = Data.Aeson.encode jsonConn
    let subflowCount = length $ subflows mptcpConn
    let tmpdir = "/tmp"
    let filename = tmpdir ++ "/" ++ "mptcp_" ++ (show $ subflowCount)  ++ "_" ++ (show $ connectionToken mptcpConn) ++ ".json"
    -- let tmpdir = getEnvDefault "TMPDIR" "/tmp"
    infoM "main" $ "Saving to " ++ filename

    -- throws upon error
    Data.ByteString.Lazy.writeFile filename bs


    -- TODO to keep it simple it should return a list of CWNDs to apply
    -- readProcessWithExitCode  binary / args / stdin
    (exitCode, stdout, stderrContent) <- readProcessWithExitCode "./hs/fake_solver" [filename, show subflowCount] ""

    putStrLn $ "exitCode: " ++ show exitCode
    putStrLn $ "stdout:\n" ++ stdout
    -- http://hackage.haskell.org/package/base/docs/Text-Read.html
    let values = (case exitCode of
        -- for now simple, we might read json afterwards
                      ExitSuccess -> (readMaybe stdout) :: Maybe [Word32]
                      ExitFailure val -> error $ "stdout:" ++ stdout ++ " stderr: " ++ stderrContent
                      )
    return values

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

--
inspectAnswers :: [GenlPacket NoData] -> IO ()
inspectAnswers packets = do
  mapM_ inspectAnswer packets
  putStrLn "Finished inspecting answers"

-- showPacketCustom :: GenlPacket NoData -> String
-- showPacketCustom pkt = let
--   hdr = (genlDataHeader pkt )
--   in showPacket pkt

showHeaderCustom :: GenlHeader -> String
showHeaderCustom = show

inspectAnswer :: GenlPacket NoData -> IO ()
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






-- |Deal with events for already registered connections
-- Warn: MPTCP_EVENT_ESTABLISHED registers a "null" interface
-- TODO return a Maybe ?
-- or a list of packets to send
-- TODO return the MVar ?
--
-- availablePaths <- readMVar globalInterfaces
-- TODO put globalInterfaces in MyState ?
    -- let (MptcpSocket mptcpSockRaw fid) = mptcpSock
    -- let pkts = (onMasterEstablishement pathManager) mptcpSock mptcpConn availablePaths

    -- putStrLn "List of requests made on new master:"
    -- mapM_ (sendPacket $ mptcpSockRaw) pkts

dispatchPacketForKnownConnection :: MptcpSocket
                                    -> MptcpConnection
                                    -> MptcpGenlEvent
                                    -> Attributes
                                    -> AvailablePaths
                                    -- -> Map.Map MptcpAttr MptcpAttribute
                                    -- -> MptcpConnection
                                    -> (Maybe MptcpConnection, [MptcpPacket])
dispatchPacketForKnownConnection mptcpSock con event attributes availablePaths = let
        token = connectionToken con
        subflow = subflowFromAttributes attributes
    in
    case event of

      -- let the Path manager kick in
      MPTCP_EVENT_ESTABLISHED -> let
              -- onMasterEstablishement mptcpSock 
              -- Needs IO because of NetworkInterface
              newPkts = (onMasterEstablishement pathManager) mptcpSock con availablePaths
          in
              (Just con, newPkts)

      -- TODO trigger the pathManager again, fix the remote interpretation
      MPTCP_EVENT_ANNOUNCED -> let
          -- what if it's local
            remId = remoteIdFromAttributes attributes
            -- TODO 
            -- newConn = mptcpConnAddRemoteId con remId
            newConn = con
          in
            (Just newConn, [])

      MPTCP_EVENT_CLOSED -> (Nothing, [])

      MPTCP_EVENT_SUB_ESTABLISHED -> let
                newCon = mptcpConnAddSubflow con subflow
            in
                (Just newCon,[])
        -- let newState = oldState
        -- putMVar con newCon
        -- let newState = oldState { connections = Map.insert token newCon (connections oldState) }
        -- TODO we should insert the
        -- newConn <-
        -- return newState

      -- TODO remove
      MPTCP_EVENT_SUB_CLOSED -> let
              newCon = mptcpConnRemoveSubflow con subflow
            in
              (Just newCon, [])

      -- MPTCP_CMD_EXIST -> con

      _ -> error $ "should not happen " ++ show event


acceptConnection :: TcpConnection -> Bool
-- acceptConnection subflow = subflow `notElem` filteredConnections
acceptConnection subflow = True

-- |
mapSubflowToInterfaceIdx :: IP -> IO (Maybe Word32)
mapSubflowToInterfaceIdx ip = do

  res <- tryReadMVar globalInterfaces
  case res of
    Nothing -> error "Couldn't access the list of interfaces"
    Just interfaces -> return $ mapIPtoInterfaceIdx interfaces ip



-- TODO reestablish later on
-- open additionnal subflows
-- createNewSubflows :: MptcpSocket -> MptcpConnection -> IO ()
-- createNewSubflows mptcpSock mptcpConn = do
--     availablePaths <- readMVar globalInterfaces
--     let (MptcpSocket mptcpSockRaw fid) = mptcpSock
--     let pkts = (onMasterEstablishement pathManager) mptcpSock mptcpConn availablePaths

--     putStrLn "List of requests made on new master:"
--     mapM_ (sendPacket $ mptcpSockRaw) pkts


-- Maybe
registerMptcpConnection :: MyState -> MptcpToken -> TcpConnection -> IO MyState
registerMptcpConnection oldState token subflow = let
        (MyState mptcpSock conns) = oldState
    in
    if acceptConnection subflow == False
        then do
            putStrLn $ "filtered out connection" ++ show subflow
            return oldState
        else (do
                -- should we add the subflow yet ? it doesn't have the correct interface idx
                mappedInterface <- mapSubflowToInterfaceIdx (srcIp subflow)
                let fixedSubflow = subflow { subflowInterface = mappedInterface }
                -- let newMptcpConn = (MptcpConnection token [] Set.empty Set.empty)
                let newMptcpConn = mptcpConnAddSubflow (MptcpConnection token Set.empty Set.empty Set.empty) fixedSubflow

                newConn <- newMVar newMptcpConn
                putStrLn $ "Connection established !!\n"

                -- create a new
                sockMetrics <- makeMetricsSocket
                -- start monitoring connection
                threadId <- forkOS (startMonitorConnection mptcpSock sockMetrics newConn)

                putStrLn $ "Inserting new MVar "
                let newState = oldState {
                    connections = Map.insert token (threadId, newConn) (connections oldState)
                }
                return newState)

-- |Treat MPTCP events depending on if the connection is known or not
--
dispatchPacket :: MyState -> MptcpPacket -> IO MyState
dispatchPacket oldState (Packet hdr (GenlData genlHeader NoData) attributes) = let
        cmd = toEnum $ fromIntegral $ genlCmd genlHeader
        (MyState mptcpSock conns) = oldState
        (MptcpSocket mptcpSockRaw fid) = mptcpSock

        -- i suppose token is always available right ?
        token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes
        maybeMatch = Map.lookup token (connections oldState)
    in do
        putStrLn $ "Fetching available paths"
        availablePaths <- readMVar globalInterfaces

        putStrLn $ "dispatch cmd " ++ show cmd ++ " for token " ++ show token

        case maybeMatch of
            -- Unknown token
            Nothing -> do

                putStrLn $ "Unknown token/connection " ++ show token
                case cmd of

                  MPTCP_EVENT_ESTABLISHED -> do
                      putStrLn "Ignoring Creating EVENT"
                                  -- let newMptcpConn = (MptcpConnection token [] Set.empty Set.empty)
                      return oldState

                  MPTCP_EVENT_CREATED -> let
                      subflow = subflowFromAttributes attributes
                    in
                      registerMptcpConnection oldState token subflow
                  _ -> return oldState

            Just (threadId, mvarConn) -> do
                putStrLn $ "MATT: Received request for a known connection "
                mptcpConn <- takeMVar mvarConn

                putStrLn $ "Forwarding to dispatchPacketForKnownConnection "
                case dispatchPacketForKnownConnection mptcpSock mptcpConn cmd attributes availablePaths of
                  (Nothing, _) -> do
                        putStrLn $ "Killing thread " ++ show threadId
                        killThread threadId
                        return $ oldState { connections = Map.delete token (connections oldState) }

                  (Just newConn, pkts) -> do
                        putStrLn "putting mVar"
                        putMVar mvarConn newConn
                        -- TODO update state

                        putStrLn "List of requests made on new master:"
                        mapM_ (\pkt -> sendPacket mptcpSockRaw (trace ("TOTO" ++ show pkt) pkt)) pkts
                        let newState = oldState {
                            connections = Map.insert token (threadId, mvarConn) (connections oldState)
                        }
                        return newState

                -- case cmd of

                --     MPTCP_EVENT_CREATED -> error "We should not receive MPTCP_EVENT_CREATED from here !!!"
                --     MPTCP_EVENT_SUB_CLOSED -> do
                --         putStrLn $ "SUBFLOW WAS CLOSED"
                --         return oldState

                --     MPTCP_EVENT_ANNOUNCED -> do
                --         -- what if it's local
                --           case makeAttributeFromMaybe MPTCP_ATTR_REM_ID attributes of
                --               Nothing -> con
                --               Just (RemoteLocatorId remId) -> do
                --                   mptcpConnAddRemoteId con remId
                --                   createNewSubflows mptcpSock mptcpConn
                --               _ -> error "Wrong translation"

                --     MPTCP_EVENT_CLOSED -> do
                --         putStrLn $ "Killing thread " ++ show threadId
                --         killThread threadId
                --         return $ oldState { connections = Map.delete token (connections oldState) }
                --     MPTCP_EVENT_ESTABLISHED -> do
                --         putStrLn "Connexion established"
                        -- mptcpConn <- readMVar mvarConn
                        -- createNewSubflows mptcpSock mptcpConn >> return oldState

                -- TODO update connection
                    -- TODO filter first
                    -- _ -> do

                    --     putStrLn $ "Forwarding to dispatchPacketForKnownConnection "
                        -- -- TODO convert attributes
                        -- -- convertAttributesIntoMap 
                        -- let newConn = dispatchPacketForKnownConnection mptcpSock mptcpConn cmd attributes



dispatchPacket s (DoneMsg err) =
  putStrLn "Done msg" >> return s


-- EOK shouldn't be an ErrorMsg when it receives EOK ?
dispatchPacket s (ErrorMsg hdr errCode errPacket) = do
  if errCode == 0 then
    putStrLn $ "Received acknowledgement for " ++ show hdr
  else
    putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n" ++ show errPacket

  return s

-- ++ show errPacket
showError :: Show a => Packet a -> IO ()
showError (ErrorMsg hdr errCode errPacket) =
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

inspectResult :: MyState -> Either String MptcpPacket -> IO MyState
inspectResult myState result = case result of
      Left ex -> putStrLn ("An error in parsing happened" ++ show ex) >> return myState
      Right myPack -> dispatchPacket myState myPack


-- |Infinite loop basically
doDumpLoop :: MyState -> IO MyState
doDumpLoop myState = do
    let (MptcpSocket simpleSock fid) = socket myState

    results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]

    -- TODO retrieve packets
    -- here we should update the state according
    -- mapM_ (inspectResult myState) results
    -- (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
    modifiedState <- foldM inspectResult myState results

    doDumpLoop modifiedState

-- regarder dans query/joinMulticastGroup/recvOne
listenToEvents :: MptcpSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents (MptcpSocket sock fid) my_group = do
  -- joinMulticastGroup  returns IO ()
  -- TODO should check it works correctly !
  joinMulticastGroup sock (grpId my_group)
  putStrLn $ "Joined grp " ++ grpName my_group
  let globalState = MyState mptcpSocket Map.empty
  _ <- doDumpLoop globalState
  putStrLn "end of listenToEvents"
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



dumpExtensionAttribute :: Int -> ByteString -> SockDiagExtension
dumpExtensionAttribute attrId value = let
        eExtId = (toEnum attrId :: SockDiagExtensionId)
        ext_m = loadExtension attrId value
    in
        case ext_m of
            Nothing -> error $ "Could not load " ++ show eExtId ++ " (unsupported)\n"
            Just ext -> ext
            -- traceId (show eExtId) ++ " " ++ showExtension ext ++ " \n"

loadExtensionsFromAttributes :: Attributes -> [SockDiagExtension]
loadExtensionsFromAttributes attrs =
    let
        -- $ loadExtension
        mapped = Map.foldrWithKey (\k v -> ([dumpExtensionAttribute k v] ++ )) [] attrs
    in
        mapped


{- Parses the requested informations
-}
-- (TcpConnection, [SockDiagExtension])
inspectIDiagAnswer :: Packet InetDiagMsg -> Maybe SockDiagMetrics
inspectIDiagAnswer (Packet hdr cus attrs) = let
    con = connectionFromDiag  cus
  in
    Just $ SockDiagMetrics con (loadExtensionsFromAttributes attrs)
    -- putStrLn ("Idiag custom " ++ show cus) >>
    -- putStrLn ("Idiag header " ++ show hdr) >>
    -- putStrLn (showExtensionAttributes attrs)
inspectIDiagAnswer p = Nothing

-- inspectIDiagAnswer (DoneMsg err) = putStrLn "DONE MSG"
-- (GenlData NoData)
-- inspectIdiagAnswer (Packet hdr (GenlData ghdr NoData) attributes) = putStrLn $ "Inspecting answer:\n"
-- inspectIdiagAnswer (Packet _ (GenlData hdr NoData) attributes) = let
--     cmd = genlCmd hdr
--   in
--     putStrLn $ "Inspecting answer custom:\n" ++ showHeaderCustom hdr
--             ++ "Supposing it's a mptcp command: " ++ dumpCommand ( toEnum $ fromIntegral cmd)


-- |Convenience wrapper
data SockDiagMetrics = SockDiagMetrics {
  subflowSubflow :: TcpConnection
  , sockdiagMetrics :: [SockDiagExtension]
}


instance ToJSON SockDiagExtension where
  -- tcpi_rtt / tcpi_rttvar / tcpi_snd_ssthresh / tcpi_snd_cwnd 
  -- tcpi_state , tcpi_rto
  toJSON (arg@DiagTcpInfo {} )  =  object [
      "rttvar" .= tcpi_rtt_var arg,

      "rtt" .= tcpi_rtt arg,
      "snd_cwnd" .= tcpi_snd_cwnd arg
      "tcpi_total_retrans"  .= tcpi_snd_cwnd arg

      ]
  toJSON (TcpVegasInfo _ _ rtt minRtt) = object [ "rtt" .= toJSON (rtt :: Word32) ]
  toJSON (CongInfo cc) = object [ "cc" .= toJSON (cc) ]
  toJSON (Meminfo wmem rmem _ _) = object [
      "wmem" .= "wmem"
      ]

-- TODO merge
--
instance ToJSON SockDiagMetrics where
  -- attributes of array
  -- foldr over array of extensions
  toJSON (SockDiagMetrics sf metrics) =

    object [
    (pack (nameFromTcpConnection $ sf) .= object [
      "cwnd" .= toJSON (20 :: Int),
      -- for now hardcode mss ? we could set it to one to make
      "mss" .= toJSON (1500 :: Int),
      "var" .= toJSON (10 :: Int),
      "fowd" .= toJSON (10 :: Int),
      "bowd" .= toJSON (10 :: Int),
      "loss" .= toJSON (0.5 :: Float)
      "rto" .= toJSON (0.5 :: Float)
      -- This is an user preference, that should be pushed when calling mptcpnumerics
      -- , "contribution": 0.5
    ])
    ]

-- |Updates the list of interfaces
-- should run in background
trackSystemInterfaces :: IO()
trackSystemInterfaces = do
  -- check routing information
  routingSock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
  let cb = NLS.NLCallback (pure ()) (handleAddr . runGet getGenPacket)
  NLS.nlPostMessage routingSock queryAddrs cb
  NLS.nlWaitCurrent routingSock
  dumpSystemInterfaces


-- | Remove ?
inspectIdiagAnswers :: [Packet InetDiagMsg] -> [Maybe SockDiagMetrics]
inspectIdiagAnswers packets =
  map inspectIDiagAnswer packets

{-| The MptcpSocket doesn't need to be global
-}
-- withFormatter :: GenericHandler Handle -> GenericHandler Handle
-- withFormatter handler = setFormatter handler formatter
--     -- http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
--     where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do

  -- SETUP LOGGING (https://gist.github.com/ijt/1052896)
  myStreamHandler <- streamHandler stderr INFO
  -- let myStreamHandler' = withFormatter myStreamHandler
  -- let rootLog = rootLoggerName
  -- updateGlobalLogger rootLog (setLevel INFO)
  updateGlobalLogger "main" (setLevel DEBUG)

  infoM "main" "Parsing command line..."
  options <- execParser opts

  infoM "main" "Creating MPTCP netlink socket..."


  -- let jsonConn = toJSON $ MptcpConnection 32 Set.empty Set.empty Set.empty
  -- let objToMerge = object [ "toto" .= toJSON ("value" :: Value) ]
  -- let merged = lodashMerge jsonConn objToMerge
  -- let bs = Data.Aeson.encode $ merged
  -- putStrLn $ show bs

  -- add the socket too to an MVar ?
  (MptcpSocket sock  fid) <- makeMptcpSocket
  putMVar globalMptcpSock (MptcpSocket sock  fid)

  infoM "main" "Now Tracking system interfaces..."
  putMVar globalInterfaces Map.empty
  routeNl <- forkIO trackSystemInterfaces

  putStr "socket created. MPTCP Family id " >> Prelude.print fid
  -- putStr "socket created. tcp_metrics Family id " >> print fidMetrics
  -- That's what I should use in fact !! (Word16, [CtrlAttrMcastGroup])
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  -- Each netlink family has a set of 32 multicast groups
  mcastMptcpGroups <- getMulticastGroups sock fid
  mapM_ Prelude.print mcastMptcpGroups

  mapM_ (listenToEvents (MptcpSocket sock fid)) mcastMptcpGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastMptcpGroups )
  putStrLn "finished"

