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

import Prelude hiding (concat)
import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)

-- For TcpState, FFI generated
import Generated
import IDiag
import Net.Mptcp
import Net.IP
import Net.IPv4 hiding (print)
import Net.IPAddress

-- for replicateM
-- import Control.Monad
import Data.Maybe ()
import Data.Foldable ()
import Foreign.C.Types (CInt)
import Foreign.C.Error
import System.Linux.Netlink hiding (makeSocket)
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.Helpers
import System.Log.FastLogger
import System.Linux.Netlink.GeNetlink.Control

import System.Process
import System.Exit
import Data.Word (Word16, Word32)
-- import qualified Data.Bits as Bits -- (shiftL, )
-- import Data.Bits ((.|.))
-- import Data.Serialize.Get
import Data.Serialize.Put
-- import Data.Either (fromRight)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (writeFile)
-- import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map

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

iperfClientPort :: Word16
iperfClientPort = 5500

iperfServerPort :: Word16
iperfServerPort = 5500

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



-- inspired by makeNL80211Socket Create a 'NL80211Socket' this opens a genetlink
-- socket and gets the family id
-- TODO should record the token too
-- TODO should join the group !!
makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket = do
    -- for legacy reasons this opens a route socket
  sock <- makeSocket
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
updateCwndCap :: IO ()
updateCwndCap = do
    -- TODO fix con
    let fakeCon = TcpConnection (fromIPv4 localhost) (fromIPv4 localhost) 0 0
    putStrLn "Reading metrics sock Mvar..."
    sockMetrics <- readMVar globalMetricsSock

    putStrLn "Reading mptcp sock Mvar..."
    mptcpSock <- readMVar globalMptcpSock
    putStrLn "Finished reading"
    -- sendmsg genQueryPacket fakeCon
    let queryPkt = genQueryPacket Nothing [TcpListen, TcpEstablished] [InetDiagCong, InetDiagInfo, InetDiagMeminfo]
    let (MptcpSocket testSock familyId ) = mptcpSock
    sendPacket sockMetrics queryPkt
    putStrLn "Sent the TCP SS request"

    -- exported from my own version !!
    -- recvMulti sockMetrics >>= inspectIdiagAnswers
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
            , SubflowInterface localhostIntfIdx
            -- , SubflowInterface localhostIntfIdx
            ]
    -- putStrLn "while waiting for a real implementation"

-- Use a Mvar here to
startMonitorConnection :: MptcpSocket -> MVar MptcpConnection -> IO ()
startMonitorConnection mptcpSock mConn = do
    -- ++ show token
    let (MptcpSocket sock familyId) = mptcpSock
    putStrLn "Start monitoring connection..."
    -- as long as conn is not empty we keep going ?
    -- for this connection
    -- query metrics for the whole MPTCP connection
    con <- readMVar mConn
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

    putStrLn "Requesting to set cwnds..."
    -- TODO capCwndAttrs capCwndPkt 
    -- KISS for now (capCwndPkt mptcpSock )
    -- zip caps (subflows con) 
    let attrsList = map (\(cwnd, sf) -> capCwndAttrs token sf cwnd ) (zip cwnds (subflows con))
    -- >> map (capCwndPkt mptcpSock ) >>= putStrLn "toto"
    -- query sock capCwndPkt >>= inspectAnswers
    let cwndPackets = map (capCwndPkt mptcpSock ) attrsList
    -- map >>= inspectAnswers

    -- then we should send a request for each cwnd

    sleepMs 5000
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
    -- need to start a process
    -- should return a bytestring
    let bs = Data.Aeson.encode con
    let subflowCount = length $ subflows con
    let filename = "mptcp_" ++ (show $ subflowCount)  ++ "_" ++ (show $ connectionToken con) ++ ".json"
    -- encode token in filename
    -- fromMaybe $
    Data.ByteString.Lazy.writeFile filename bs

    -- eitherDecode
    -- TODO run readProcessWithExitCode instead
    -- FilePath -> [String] -> String -> IO (ExitCode, String, String)

    -- TODO to keep it simple it should return a list of CWNDs to apply
    -- readProcessWithExitCode  binary / args / stdin
    (exitCode, stdout, stderr) <- readProcessWithExitCode "fake_solver" [filename, show subflowCount] ""
    case exitCode of
        -- successful
        -- print result 
        -- for now simple, we might read json afterwards
        ExitSuccess -> return (read stdout :: [Word32])
        ExitFailure val -> return []

-- type Attributes = Map Int ByteString
-- the library contains showAttrs / showNLAttrs
showAttributes :: Attributes -> String
showAttributes attrs =
  let
    -- f k v = [show k, " = ", show v]
    -- mapped = Map.mapWithKey f attrs
    mapped = Map.foldrWithKey (\k v -> (dumpAttribute k v ++)  ) "\n " attrs
  in
    -- putStrLn $ intercalate "," $ mapped
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


--
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


-- |Class to implement different path managers
-- class PathManager a where
--     onNewSubflow :: TcpConnection -> [MptcpPacket]


-- unknownConnectionEvent :: M
-- unknownConnectionMonitor :: MptcpToken -> MptcpPacket
-- pass [MptcpAttribute] instead ?
-- 
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
        -- putStrLn "New address announced"
        -- case maybeConn of
        --     Nothing -> putStrLn "No connection with this token" >> return oldState
        --     Just mConn -> _
                -- case mmakeAttributeFromMaybe MPTCP_ATTR_REM_ID attrs of 
                --     Nothing -> return oldState
                --     Just localId ->
                --     putStrLn "Found a match"
                --     -- swapMVar / withMVar / modifyMVar
                --     mptcpConn <- takeMVar mConn
                --     -- TODO we should trigger an update in the CWND
                --     let newCon = mptcpConnAddLocalId mptcpConn 
                --     let newState = oldState
        -- return oldState

      MPTCP_EVENT_CLOSED -> do
        -- putStrLn $ "Connection closed, deleting token " ++ show token
        -- let newState = oldState { connections = Map.delete token (connections oldState) }
        -- TODO we should kill the thread with killThread or empty the mVar !!
        -- let newState = oldState
        -- putStrLn $ "New state"
        -- return newState
        con

      MPTCP_EVENT_SUB_ESTABLISHED ->  let
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
            Nothing -> case cmd of
                MPTCP_EVENT_CREATED ->
                    let subflow = subflowFromAttributes attributes
                    in
                    if subflow `notElem` filteredConnections
                        then do
                            putStrLn $ "filtered out connection" ++ show subflow 
                            putStrLn $ "it was compared with " ++ show authorizedCon1 
                            return oldState
                        else (do
                                let newMptcpConn = MptcpConnection token [ subflow ] [] []
                                newConn <- newMVar newMptcpConn
                                putStrLn $ "Connection created !!\n" ++ showAttributes attributes
                                -- onNewConnection mptcpSock attributes
                                handle <- forkOS (startMonitorConnection mptcpSock newConn)
                                -- r <- createProces $ startMonitor token
                                -- putStrLn $ "Connection created !!\n" ++ show subflow
                                let newState = oldState { connections = Map.insert token (handle, newConn) (connections oldState) }
                                return newState)

                _ -> return oldState

            Just (threadId, mvarConn) -> case cmd of
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

inspectIDiagAnswer :: Packet InetDiagMsg -> IO ()
inspectIDiagAnswer (Packet hdr cus attrs) =
   putStrLn $ "Idiag answer" ++ showAttributes attrs
inspectIDiagAnswer p = putStrLn $ "test" ++ showPacket p

-- inspectIDiagAnswer (DoneMsg err) = putStrLn "DONE MSG"
-- (GenlData NoData)
-- inspectIdiagAnswer (Packet hdr (GenlData ghdr NoData) attributes) = putStrLn $ "Inspecting answer:\n"
-- inspectIdiagAnswer (Packet _ (GenlData hdr NoData) attributes) = let
--     cmd = genlCmd hdr
--   in
--     putStrLn $ "Inspecting answer custom:\n" ++ showHeaderCustom hdr
--             ++ "Supposing it's a mptcp command: " ++ dumpCommand ( toEnum $ fromIntegral cmd)


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

  -- globalState <- newIORef mempty
  -- globalState <- newEmptyMVar
  -- putStrLn "dumping important values:"
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

