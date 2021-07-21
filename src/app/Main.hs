{-|
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

This userspace program is a complement to the linux MPTCP kernel developed at
https://github.com/multipath-tcp/mptcp

The main daemon monitors MPTCP connections and for each new connection assigns a thread
to mino.


The daemon main thread has 2 roles:
- monitors interface change (network interface addition/deletion)
- listens to

Monitors new MPTCP connections and runs a specific monitor instance
To interact
GENL_ADMIN_PERM
The operation requires the CAP_NET_ADMIN privilege
iproute2/misc/ss.c to see how `ss` utility interacts with the kernel
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Net.IP
import           Net.Mptcp
import           Net.Mptcp.Constants
import           Net.Mptcp.PathManager
import           Net.Mptcp.PathManager.Default
import           Net.SockDiag
import           Net.SockDiag.Constants
import           Net.Tcp

import Control.Monad.Trans (liftIO)
import           Control.Monad                          (foldM)
-- import           Control.Monad.Trans                    (liftIO)
import           Control.Monad.Trans.State              (State, StateT, execStateT, get, put)
import           Data.Maybe                             (catMaybes)
import           Data.Text                              (Text)
import           Foreign.C.Types                        (CInt)
import           Options.Applicative                    hiding (ErrorMsg, empty, value)
import qualified Options.Applicative                    (value)
import           Prelude                                hiding (concat, init, log)
import           Text.Read                              (readMaybe)
-- for eOK, ePERM
import           Foreign.C.Error
-- import qualified System.Linux.Netlink as NL
import           System.Linux.Netlink                   as NL
import           System.Linux.Netlink.Constants         as NLC
import           System.Linux.Netlink.GeNetlink         as GENL
-- import System.Linux.Netlink.Constants (eRTM_NEWADDR)
-- import System.Linux.Netlink.Helpers
-- import System.Log.FastLogger
import           System.Linux.Netlink.GeNetlink.Control
import qualified System.Linux.Netlink.Route             as NLR
import qualified System.Linux.Netlink.Simple            as NLS

import           Data.Word                              (Word32)
import           System.Exit
import           System.Process
-- import qualified Data.Bits as Bits -- (shiftL, )
-- import Data.Bits ((.|.))
import           Data.Serialize.Get                     (runGet)
import           Data.Serialize.Put
-- import Data.Either (fromRight)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set
import qualified Data.Text as TS
import           Control.Concurrent
import           Data.Bits                              (Bits (..))
-- import           Debug.Trace
-- import System.IO.Unsafe
import           Data.Aeson
import           Numeric.Natural
import           System.FilePath                        ()
-- import           System.IO                              (stderr)
import           System.IO.Temp                         ()
-- to merge MptcpConnection export and Metrics
import           Data.Aeson.Extra.Merge                 (lodashMerge)
import           GHC.List                               (init)
import           Colog.Core.Severity ()

import Polysemy
import           Polysemy.Trace
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log
import Polysemy.Log.Colog (interpretLogStdout)
import GHC.Generics (Generic)
import Data.Either (fromRight)

-- for getEnvDefault, to get TMPDIR value.
-- we could pass it as an argument
-- import System.Environment.Blank(getEnvDefault)

tshow :: Show a => a -> TS.Text
tshow = TS.pack . Prelude.show

-- |Delay between 2 successful loggings
onSuccessSleepingDelayMs :: Natural
onSuccessSleepingDelayMs = 300


-- | When it couldn't set the correct value
onFailureSleepingDelay :: Natural
onFailureSleepingDelay = 100

-- |the default path manager
pathManager :: PathManager
pathManager = meshPathManager

-- |Helper to pass information across functions
data MyState = MyState {
  socket                :: MptcpSocket -- ^Socket
  -- ThreadId/MVar
  , connections         :: Map.Map MptcpToken (ThreadId, MVar MptcpConnection)
  -- |Arguments passed to the program
  , cliArguments        :: CLIArguments

  -- |Connections to accept, loaded via cli's --filter
  , filteredConnections :: Maybe [TcpConnection]
}

 -- https://stackoverflow.com/questions/3640120/combine-state-with-io-actions
type GState a = State MyState a

-- https://stackoverflow.com/questions/51407547/how-to-update-a-field-of-a-json-object
-- addJsonKey :: Data.Text.Text -> Value -> Value -> Value
-- addJsonKey key val (Object xs) = Object $ HM.insert key val xs
-- addJsonKey _ _ xs = xs


dumpCommand :: MptcpGenlEvent -> String
dumpCommand x = show x ++ " = " ++ show (fromEnum x)

dumpMptcpCommands :: MptcpGenlEvent -> String
dumpMptcpCommands MPTCP_CMD_EXIST = dumpCommand MPTCP_CMD_EXIST
dumpMptcpCommands x               = dumpCommand x ++ "\n" ++ dumpMptcpCommands (succ x)


data CLIArguments = CLIArguments {

  -- | Path to a program in charge of generating congestion window limits on a
  -- per path basis
  -- The program will be called with a json file as input and must echo on stdout
  -- an array of the form [ 10, 30, 40]
  optimizer  :: Maybe FilePath

  -- | to filter
  , filter   :: Maybe FilePath

  -- | Folder where to log files
  , out      :: FilePath

  -- , clientIP      :: IPv4
  , quiet    :: Bool

  -- Priority
  , logLevel :: Log.Severity
  }


-- loggerName :: String
-- loggerName = "main"

dumpSystemInterfaces :: IO()
dumpSystemInterfaces = do
  putStrLn "Dumping interfaces"
  -- isEmptyMVar globalInterfaces
  res <- tryReadMVar globalInterfaces
  case res of
    Nothing         -> putStrLn "No interfaces"
    Just interfaces -> Prelude.print interfaces

  putStrLn "End of dump"


sample :: Parser CLIArguments
sample = CLIArguments
      <$> (optional $ strOption
          ( long "optimizer"
          <> short 'p'
         <> help "Path to the userspace program"
         <> metavar "PROGRAM" ))
      <*> (optional $ strOption
          ( long "filter"
         <> help "Path to a json file describing a TCP connection"
         <> metavar "Filter" ))
      <*> strOption
          ( long "out"
          <> short 'o'
         <> help "Where to store the files"
         <> showDefault
         <> Options.Applicative.value "/tmp"
         <> metavar "PROGRAM" )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "log-level"
         <> help "Log level"
         <> showDefault
         <> Options.Applicative.value Log.Info
         <> metavar "LOG_LEVEL"
        )

deriving instance Read Log.Severity

readConnectionRole :: ReadM Log.Severity
readConnectionRole = eitherReader $ \arg -> case reads arg of
  [(a, "")] -> return $ a
  -- [("client", "")] -> return $ RoleClient
  _ -> Left $ "readConnectionRole: cannot parse value `" ++ arg ++ "`"


opts :: ParserInfo CLIArguments
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
    Nothing  -> error $ "Could not find family " ++ mptcpGenlName
    Just fid -> return  (MptcpSocket sock fid)



makeMetricsSocket :: IO NetlinkSocket
makeMetricsSocket = makeSocketGeneric eNETLINK_SOCK_DIAG


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs :: Natural -> IO()
sleepMs n = threadDelay $ (fromIntegral n :: Int) * 1000


-- | here we may want to run mptcpnumerics to get some results
updateSubflowMetrics :: NetlinkSocket -> TcpConnection -> IO SockDiagMetrics
updateSubflowMetrics sockMetrics subflow = do
    putStrLn "Updating subflow metrics"
    let queryPkt = genQueryPacket (Right subflow) [TcpListen, TcpEstablished]

         [InetDiagCong, InetDiagInfo, InetDiagMeminfo]
    sendPacket sockMetrics queryPkt

    putStrLn "Sent the TCP SS request"
    putStrLn "Starting inspecting answers"

    answers <- Main.recvMulti sockMetrics
    let metrics_m = inspectIdiagAnswers answers
    -- filter ? keep only valud ones ?
    return $ head (catMaybes metrics_m)

isFlagSet :: Bits a => a -> a -> Bool
isFlagSet f v = (f .&. v) == f

-- Copy/pasted from System.Linux.Netlink
-- |Internal function to receive multiple netlink messages
recvMulti :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Packet a]
recvMulti sock = do
    pkts <- recvOne sock
    if isMulti (first pkts)
        then if isDone (last pkts)
             -- This is fine because first would have complained before
             then return $ init pkts
             else (pkts ++) <$> Main.recvMulti sock
        else return pkts
  where
    isMulti = isFlagSet fNLM_F_MULTI . messageFlags . packetHeader
    isDone  = (== eNLMSG_DONE) . messageType . packetHeader
    first (x:_) = x
    first []    = error "Got empty list from recvOne in recvMulti, this shouldn't happen"

{- |
  Starts monitoring a specific MPTCP connection
  Maybe should expect a pathManager instance (and logger
FilePath -- ^Path towards the program to get cwnd limits
-}
startMonitorConnection ::
  (Members '[Log, Trace, Embed IO ] r) =>
  CLIArguments
  --  | elapsed time since starting the thread (very coarse approximation)
  -> Natural
  -> MptcpSocket
  -> NetlinkSocket
  -> MVar MptcpConnection -> Sem r ()
startMonitorConnection cliArgs elapsed mptcpSock sockMetrics mConn = do
    let (MptcpSocket sock _) = mptcpSock
    myId <- embed $ myThreadId
    trace $ show myId ++ ": monitoring connection at *time* " ++ show elapsed ++ " ..."
    -- as long as conn is not empty we keep going ?
    -- for this connection
    -- query metrics for the whole MPTCP connection
    mptcpConn <- embed $ readMVar mConn
    Log.trace "Showing MPTCP connection"
    Log.trace $ tshow mptcpConn <> "..."
    let _token = connectionToken mptcpConn
    let tmpdir = out cliArgs

    -- TODO this is the issue
    -- not sure it's the master with a set
    let _masterSf = Set.elemAt 0 (subflows mptcpConn)

    -- Get updated metrics
    lastMetrics <- embed $ mapM (updateSubflowMetrics sockMetrics) (Set.toList $ subflows mptcpConn)
    let filename = tmpdir ++ "/" ++ "mptcp_" ++ show (connectionToken mptcpConn) ++ "_" ++ show elapsed ++ ".json"
    -- logStatistics filename elapsed mptcpConn lastMetrics

    duration <- case optimizer cliArgs of
      Nothing -> return onSuccessSleepingDelayMs
      Just prog -> do

          Log.debug "Calling third party program"

          cwnds_m <- embed $ getCapsForConnection filename prog mptcpConn lastMetrics
          -- rename to waitingTime ? delay
          case cwnds_m of
              Nothing -> do
                  Log.error "Couldn't fetch the values"
                  return onFailureSleepingDelay
              Just cwnds -> do

                  Log.info $ "Requesting to set cwnds..." <> tshow cwnds
                  -- TODO fix
                  -- KISS for now (capCwndPkt mptcpSock )
                  let cwndPackets  = map (\(cwnd, sf) -> capCwndPkt mptcpSock mptcpConn cwnd sf) (zip cwnds (Set.toList $ subflows mptcpConn))

                  embed $ mapM_ (sendPacket sock) cwndPackets

                  return onSuccessSleepingDelayMs
    Log.debug $ "Finished monitoring token. Waiting " <> tshow duration
    embed $ sleepMs duration

    -- call ourself again
    startMonitorConnection cliArgs (elapsed + duration) mptcpSock sockMetrics mConn



{-
  | This should return a list of cwnd to respect a certain scenario
 1. save the connection to a JSON file and pass it to mptcpnumerics

-}
getCapsForConnection :: FilePath     -- ^Statistics file
                        -> FilePath  -- ^Path towards the PM optimizer
                        -> MptcpConnection
                        -> [SockDiagMetrics]
                        -> IO (Maybe [Word32])
getCapsForConnection filename prog mptcpConn metrics = do

    let subflowCount = length $ subflows mptcpConn

    -- Data.ByteString.Lazy.writeFile filename jsonBs

    -- readProcessWithExitCode  binary / args / stdin
    (exitCode, stdout, stderrContent) <- readProcessWithExitCode prog [filename, show subflowCount] ""

    -- Info
    putStrLn $ "exitCode: " ++ show exitCode
    putStrLn $ "stdout:\n" ++ stdout
    -- http://hackage.haskell.org/package/base/docs/Text-Read.html
    let values = (case exitCode of
        -- for now simple, we might read json afterwards
                      ExitSuccess     -> (readMaybe stdout) :: Maybe [Word32]
                      ExitFailure val -> error $ "stdout:" ++ stdout ++ " stderr: " ++ stderrContent
                      )
    return values

-- the library contains showAttrs / showNLAttrs
showAttributes :: Attributes -> String
showAttributes attrs =
  let
    mapped = Map.foldrWithKey (\k v -> (dumpAttribute k v ++)  ) "\n " attrs
  in
    mapped

putW32 :: Word32 -> ByteString
putW32 x = runPut (putWord32host x)


-- I want to override the GenlHeader version
newtype GenlHeaderMptcp = GenlHeaderMptcp GenlHeader
instance Show GenlHeaderMptcp where
  show (GenlHeaderMptcp (GenlHeader cmd ver)) =
    "Header: Cmd = " ++ show cmd ++ ", Version: " ++ show ver ++ "\n"

--
inspectAnswers :: [GenlPacket NoData] -> IO ()
inspectAnswers packets = do
  mapM_ inspectAnswer packets

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
    putStrLn $ show ("Inspecting answer custom:\n" ++ showHeaderCustom hdr
            ++ "Supposing it's a mptcp command: " ++ dumpCommand ( toEnum $ fromIntegral cmd))

inspectAnswer pkt = putStrLn $ "Inspecting answer:\n" ++ showPacket pkt


-- should have this running in parallel
queryAddrs :: NLR.RoutePacket
queryAddrs = NL.Packet
    (NL.Header NLC.eRTM_GETADDR (NLC.fNLM_F_ROOT .|. NLC.fNLM_F_MATCH .|. NLC.fNLM_F_REQUEST) 0 0)
    (NLR.NAddrMsg 0 0 0 0 0)
    mempty


-- |Deal with events for already registered connections
-- Warn: MPTCP_EVENT_ESTABLISHED registers a "null" interface
-- or a list of packets to send


-- TODO maybe the path manager should be part of the MptcpConnection
dispatchPacketForKnownConnection :: MptcpSocket
                                    -> MptcpConnection
                                    -> MptcpGenlEvent
                                    -> Attributes
                                    -> AvailablePaths
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


-- |Filter connections
-- This should be configurable in some way
acceptConnection :: TcpConnection -> Maybe [TcpConnection] -> Bool
acceptConnection subflow mFilteredConnections =
  case mFilteredConnections of
      Nothing       -> True
      -- or notElem
      Just filtered -> subflow `elem` filtered

-- |
mapSubflowToInterfaceIdx :: IP -> IO (Maybe Word32)
mapSubflowToInterfaceIdx ip = do

  res <- tryReadMVar globalInterfaces
  case res of
    Nothing         -> error "Couldn't access the list of interfaces"
    Just interfaces -> return $ mapIPtoInterfaceIdx interfaces ip



registerMptcpConnection :: MptcpToken -> TcpConnection -> StateT MyState IO ()
registerMptcpConnection token subflow = (do
    oldState <- get
    let (MyState mptcpSock conns cliArgs filtered) = oldState
    if acceptConnection subflow filtered == False
    then do
        -- infoM "main" $ "filtered out connection:" ++ show subflow
        return ()
    else (do
            -- putStrLn $ "accepted connection :" ++ show subflow
            -- should we add the subflow yet ? it doesn't have the correct interface idx
            mappedInterface <- liftIO $ mapSubflowToInterfaceIdx (srcIp subflow)
            let fixedSubflow = subflow { subflowInterface = mappedInterface }
            -- let newMptcpConn = (MptcpConnection token [] Set.empty Set.empty)
            let newMptcpConn = mptcpConnAddSubflow (
                    MptcpConnection token Set.empty Set.empty Set.empty (optimizer cliArgs)
                    ) fixedSubflow

            newConn <- liftIO $ newMVar newMptcpConn
            -- putStrLn $ "Connection established !!\n"

            -- create a new
            sockMetrics <- liftIO $ makeMetricsSocket
            -- start monitoring connection
            -- let threadId = undefined
            threadId <- liftIO $ forkOS (
            --   -- runLogAction @IO (contramap message logTextStdout) $ interpretDataLogColog @Message $ progData
              runM $ traceToIO $ interpretLogStdout$
                startMonitorConnection cliArgs 0 mptcpSock sockMetrics newConn
              )

            -- putStrLn $ "Inserting new MVar "
            put (oldState {
                connections = Map.insert token (threadId, newConn) (connections oldState)
            })
            ))

-- |Treat MPTCP events depending on if the connection is known or not
dispatchPacket :: MyState -> MptcpPacket -> IO MyState
dispatchPacket oldState (Packet hdr (GenlData genlHeader NoData) attributes) = let
        cmd = toEnum $ fromIntegral $ genlCmd genlHeader
        (MyState mptcpSock conns _ _) = oldState
        (MptcpSocket mptcpSockRaw fid) = mptcpSock

        -- i suppose token is always available right ?
        token :: MptcpToken
        token = case Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes of
          Nothing -> error "Could not retreive token "
          Just bstr -> fromRight (error "could not retreive token") (readToken bstr)
        maybeMatch = Map.lookup token (connections oldState)
    in do
        putStrLn "Fetching available paths"
        availablePaths <- readMVar globalInterfaces

        putStrLn $ "dispatch cmd " ++ show cmd ++ " for token " ++ show token

        case maybeMatch of
            -- Unknown token
            Nothing -> do

                putStrLn $ "Unknown token/connection " ++ show token
                case cmd of

                  MPTCP_EVENT_ESTABLISHED -> do
                      -- putStrLn "Ignoring Creating EVENT"
                                  -- let newMptcpConn = (MptcpConnection token [] Set.empty Set.empty)
                      return oldState

                  MPTCP_EVENT_CREATED -> let
                      subflow = subflowFromAttributes attributes
                    in
                      execStateT (registerMptcpConnection token subflow) oldState
                  _ -> return oldState

            Just (threadId, mvarConn) -> do
                putStrLn "MATT: Received request for a known connection "
                mptcpConn <- takeMVar mvarConn

                putStrLn "Forwarding to dispatchPacketForKnownConnection "
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
                        mapM_ (\pkt -> sendPacket mptcpSockRaw pkt) pkts
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
      Left ex      -> putStrLn ("An error in parsing happened" ++ show ex) >> return myState
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


listenToEvents :: MyState -> CtrlAttrMcastGroup -> IO ()
listenToEvents state my_group = do
  joinMulticastGroup sock (grpId my_group)
  putStrLn $ "Joined grp " ++ grpName my_group
  _ <- doDumpLoop state
  putStrLn "end of listenToEvents"
  where
    (MptcpSocket sock fid) = socket state


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


dumpExtensionAttribute :: Int -> ByteString -> SockDiagExtension
dumpExtensionAttribute attrId value = let
        eExtId = (toEnum attrId :: SockDiagExtensionId)
        ext_m = loadExtension attrId value
    in
        case ext_m of
            Nothing  -> error $ "Could not load " ++ show eExtId ++ " (unsupported)\n"
            Just ext -> ext
            -- traceId (show eExtId) ++ " " ++ showExtension ext ++ " \n"

loadExtensionsFromAttributes :: Attributes -> [SockDiagExtension]
loadExtensionsFromAttributes attrs =
    let
        mapped = Map.foldrWithKey (\k v -> ([dumpExtensionAttribute k v] ++ )) [] attrs
    in
        mapped


{- Parses the requested informations
-}
inspectIDiagAnswer :: Packet SockDiagMsg -> Maybe SockDiagMetrics
inspectIDiagAnswer (Packet hdr cus attrs) =
  Just $ SockDiagMetrics cus (loadExtensionsFromAttributes attrs)
  -- Just cus
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
  sockDiagMsg       :: SockDiagMsg
  -- subflowSubflow :: TcpConnection
  , sockdiagMetrics :: [SockDiagExtension]
} deriving Generic

-- type SockDiagExtension2 = SockDiagExtension
instance ToJSON SockDiagExtension where
  toJSON (tcpInfo@DiagTcpInfo {} )  = let
      -- rtt = tcpi_rtt tcpInfo
      tcpState = toEnum $ fromIntegral ( tcpi_state tcpInfo) :: TcpState
      -- TODO could log ca_state ?

    in
      object [
      "rttvar" .= tcpi_rttvar tcpInfo
      , "rtt_us" .= tcpi_rtt tcpInfo
      , "rto_us" .= tcpi_rto tcpInfo
      , "snd_cwnd" .= tcpi_snd_cwnd tcpInfo
      , "snd_cwnd_clamp" .= tcpi_snd_cwnd_clamp tcpInfo
      , "snd_ssthresh" .= tcpi_snd_ssthresh tcpInfo
      , "reordering"  .= tcpi_reordering tcpInfo
      , "tcp_state" .= show tcpState
      , "pacing" .= tcpi_pacing_rate tcpInfo
      , "delivery_rate" .= tcpi_delivery_rate tcpInfo
      , "app_limited" .= tcpi_delivery_rate_app_limited tcpInfo
      -- there is also a delivered_ce ?
      , "delivered" .= tcpi_delivered tcpInfo
      , "lost" .= tcpi_lost tcpInfo
      , "retrans" .= tcpi_retrans tcpInfo
      , "min_rtt" .= tcpi_min_rtt tcpInfo
      , "mtu" .= tcpi_pmtu tcpInfo
      -- TODO try converting it to str
      , "ca_state" .= tcpi_ca_state tcpInfo

      -- bytes_sent
      -- needs kernel patching
      -- , "fowd"  .= toJSON ( (fromIntegral rtt/2) :: Float)
      -- , "bowd"  .= toJSON ( (fromIntegral rtt/2) :: Float)

      , "fowd"  .= tcpi_fowd tcpInfo
      , "bowd"  .= tcpi_bowd tcpInfo


      -- , "total_retrans"  .= tcpi_total_retrans arg

      ]
  toJSON (TcpVegasInfo _ _ rtt minRtt) = object [ "rtt" .= toJSON (rtt :: Word32) ]
  toJSON (CongInfo cc) = object [ "cc" .= toJSON (cc) ]
  toJSON (DiagExtensionMemInfo wmem rmem _ _) = object [
      "wmem" .= toJSON ( wmem :: Word32 )
      , "rmem" .= toJSON ( rmem :: Word32 )
      ]
  toJSON _ = object []


instance ToJSON SockDiagMetrics where
  -- attributes of array
  -- foldr over array of extensions
  toJSON (SockDiagMetrics msg metrics) = let

      sf = connectionFromDiag msg
      tcpState = toEnum $ fromIntegral ( idiag_state msg) :: TcpState
      initialValue = object [
          "srcIp" .= toJSON (srcIp sf)
          , "dstIp" .= toJSON (dstIp sf)
          , "srcPort" .= toJSON (srcPort sf)
          , "dstPort" .= toJSON (dstPort sf)
          -- doesnt work as subflow id
          -- , "subflow_id" .= idiag_uid msg
          ]
      fn x y = lodashMerge (toJSON x) y

    in
    -- (a -> b -> b) -> b -> t a -> b
    foldr fn initialValue metrics


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
inspectIdiagAnswers :: [Packet SockDiagMsg] -> [Maybe SockDiagMetrics]
inspectIdiagAnswers packets =
  map inspectIDiagAnswer packets


main :: IO ()
main = do

  -- SETUP LOGGING (https://gist.github.com/ijt/1052896)
  -- streamHandler vs verboseStreamHandler
  -- myStreamHandler <- verboseStreamHandler stderr INFO

  putStrLn "Starting program"

  -- logTextStdout
  -- logStringStdout
  -- _ <- runM $ traceToIO $ runLogAction @IO richMessageAction program
  _ <- runM $ traceToIO $ interpretLogStdout program
  putStrLn "finished"

program :: (Members '[Log, Trace, Embed IO] r) => Sem r ()
program = do

  -- Log.info "Parsing command line..." :: TS.Text
  options <- embed $ execParser opts
  Log.info ("Creating MPTCP netlink socket..." :: Text)


  Log.info "Now Tracking system interfaces..."
  embed $ putMVar globalInterfaces Map.empty
  routeNl <- embed $ forkIO trackSystemInterfaces

  Log.debug "socket created. MPTCP Family id "

  mptcpSocket <- embed makeMptcpSocket
  let (MptcpSocket sock fid) = mptcpSocket
  mcastMptcpGroups <- embed $ getMulticastGroups sock fid
  embed $ mapM_ Prelude.print mcastMptcpGroups


  -- use fmap instead
  filteredConns <- case Main.filter options of
      Nothing -> return Nothing
      Just filename -> do
          Log.info ("Loading connections whitelist from " <> tshow filename <> "...")
          filteredConnectionsStr <- embed $ BL.readFile filename
          case Data.Aeson.eitherDecode filteredConnectionsStr of
          -- case Data.Aeson.eitherDecode "[]" of
            Left errMsg -> error ("Failed loading " ++ filename ++ ":\n" ++ errMsg)
            Right list  -> return list

  Log.info ("Loading connections whitelisted connections..." <> (tshow filteredConns))

  let globalState = MyState mptcpSocket Map.empty options filteredConns

  embed $ mapM_ (listenToEvents globalState) mcastMptcpGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastMptcpGroups )
