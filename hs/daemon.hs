{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
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

http://kristrev.github.io/2013/07/26/passive-monitoring-of-sockets-on-linux

iproute2/misc/ss.c to see how `ss` utility interacts with the kernel
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/process-1.6.5.0/System-Process.html

- https://lotz84.github.io/haskellbyexample/ex/timers
-}

-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)
import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)

-- For TcpState, FFI generated
import Generated
import IDiag
import Mptcp ()

-- for replicateM
-- import Control.Monad
import Data.Maybe
import Data.Foldable (foldl')
import Foreign.C.Types (CInt)
import Foreign.C.Error
import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, Packet(..))
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.Helpers
import System.Log.FastLogger
import System.Linux.Netlink.GeNetlink.Control

import System.Process
import Data.Word (Word8, Word16, Word32)
import qualified Data.Bits as Bits -- (shiftL, )
import Data.Bits ((.|.))

-- imported from cereal  hiding (runGet)
import Data.Serialize.Get
import Data.Serialize.Put

-- import Data.Either (fromRight)

import Data.ByteString (ByteString, unpack)

-- https://hackage.haskell.org/package/bytestring-conversion-0.1/candidate/docs/Data-ByteString-Conversion.html#t:ToByteString
-- contains ToByteString / FromByteString
import Data.ByteString.Conversion
import Debug.Trace
-- import Control.Exception


import qualified Data.Map as Map
-- import Data.Map (fromList, lookup, toList, Map)
-- import Data.ByteString.Char8 as C8 hiding (putStrLn, putStr)
 -- (unpack)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct

-- iproute uses this seq number
-- #define MAGIC_SEQ 123456
magicSeq :: Word32
magicSeq = 123456



iperfHardcodedSrcPort :: Word16
iperfHardcodedSrcPort = 5500


monitorConnection :: MptcpToken
monitorConnection token =
  putStrLn "Monitoring a connection"


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


-- https://stackoverflow.com/questions/18606827/how-to-write-customised-show-function-in-haskell
-- TODO could use templateHaskell
-- TODO les generer via le netlink helper mkIncludeBlock
-- look at generate.hs
-- instance Show MptcpPacket where
--   show MPTCP_EVENT_CREATED = "MPTCP_EVENT_CREATED"
--   show x = x
dumpCommand :: MptcpGenlEvent -> String
dumpCommand x = show x ++ " = " ++ show (fromEnum x)
-- dumpCommand MPTCP_CMD_UNSPEC  = " MPTCP_CMD_UNSPEC  0"
-- dumpCommand MPTCP_EVENT_SUB_ERROR = show MPTCP_EVENT_SUB_ERROR + show fromEnum MPTCP_EVENT_SUB_ERROR

dumpMptcpCommands :: MptcpGenlEvent -> String
dumpMptcpCommands MPTCP_CMD_EXIST = dumpCommand MPTCP_CMD_EXIST
dumpMptcpCommands x = dumpCommand x ++ "\n" ++ dumpMptcpCommands (succ x)




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

--createProcess 
startMonitor :: MptcpToken -> CreateProcess
startMonitor token =
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


runMptcpNumerics :: IO String
runMptcpNumerics  =
  -- TODO run readProcessWithExitCode instead
  readProcess "seq" ["1", "10"] ""


-- inspectPacket :: GenlPacket NoData -> IO ()
-- inspectPacket = Prelude.putStrLn show
-- inspectPacket packet = Prelude.putStrLn $show packet


-- TODO merge default attributes
-- todo pass a list of (Int, Bytestring) and build the map with fromList ?
getRequestPacket :: Word16 -> MptcpGenlEvent -> Bool -> Attributes -> GenlPacket NoData
getRequestPacket fid cmd dump attrs =
  let
    -- The message type/ flag / sequence number / pid  (0 => from the kernel)
    -- https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/netlink.h#L54
    myHeader = Header (fromIntegral fid) (flags .|. fNLM_F_ACK) 0 0
    geheader = GenlHeader word8Cmd mptcpGenlVer
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
    word8Cmd = fromIntegral (fromEnum cmd) :: Word8
  in

    -- inspired by System/Linux/Netlink/GeNetlink/NL80211.hs
    -- Packet header (GenlData geheader NoData80211) attrs
    Packet myHeader (GenlData geheader NoData) attrs

--   System.Linux.Netlink.ErrorMsg -> error "error msg"
-- clampWindowRequest



-- TODO convert port




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

-- getMptcpAttribute :: MptcpAttr -> Attributes -> MptcpAttribute
-- getMptcpAttribute attr 

-- TODO prefix with --cmd
-- mptcp_nl_genl_create
-- sport/backup/intf are optional
-- family AF_INET/loc id/remid are not
createNewSubflow :: MptcpSocket -> MptcpToken -> Attributes -> (GenlPacket NoData)
createNewSubflow (MptcpSocket sock fid) token attrs = let
    -- localId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_LOC_ID) attrs
    -- remoteId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_REM_ID) attrs
        -- [ MptcpToken ]

    -- TODO put attributes to create a new subflow

    -- attributes = Map.empty
    attributes = toAttributes [
        (MptcpAttrToken token),
        (LocalLocatorId 0) , (RemoteLocatorId 0)
        , SubflowFamily eAF_INET ]
    cmd = MPTCP_CMD_SUB_CREATE
    pkt = getRequestPacket fid cmd False attributes
  in
    pkt

  -- putStrLn "createNewSubflow TODO"
  -- if (!info->attrs[MPTCP_ATTR_TOKEN] || !info->attrs[MPTCP_ATTR_FAMILY] ||
  --     !info->attrs[MPTCP_ATTR_LOC_ID] || !info->attrs[MPTCP_ATTR_REM_ID])


putW32 :: Word32 -> ByteString
putW32 x = runPut (putWord32host x)

-- inspired by netlink cATA :: CtrlAttribute -> (Int, ByteString)
attrToPair :: MptcpAttribute -> (Int, ByteString)
attrToPair (MptcpAttrToken token) = (fromEnum MPTCP_ATTR_TOKEN, runPut $ putWord32host token)
attrToPair (RemoteLocatorId loc) = (fromEnum MPTCP_ATTR_REM_ID, runPut $ putWord8 loc)
attrToPair (LocalLocatorId loc) = (fromEnum MPTCP_ATTR_REM_ID, runPut $ putWord8 loc)
attrToPair (SubflowFamily fam) = let
        fam8 = (fromIntegral $ fromEnum fam) :: Word8
    in (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord8 fam8)

attrToPair ( MptcpIntf idx) = (fromEnum MPTCP_ATTR_IF_IDX, runPut $ putWord8 idx)
attrToPair ( MptcpAddr addr) = (fromEnum MPTCP_ATTR_SADDR4, addr)
-- attrToPair _ = error "unsupported"


toAttributes :: [MptcpAttribute] -> Attributes
toAttributes attrs = Map.fromList $map attrToPair attrs

-- removeSubflow :: MptcpSocket -> MptcpToken -> LocId -> IO [GenlPacket NoData]
-- removeSubflow (MptcpSocket socket fid) token locId = let

-- I want to override the GenlHeader version
newtype GenlHeaderMptcp = GenlHeaderMptcp GenlHeader
instance Show GenlHeaderMptcp where
  show (GenlHeaderMptcp (GenlHeader cmd ver)) =
    "Header: Cmd = " ++ show cmd ++ ", Version: " ++ show ver ++ "\n"


-- removeLocId :: MptcpSocket -> MptcpAttribute -> IO [GenlPacket NoData]
-- removeLocId (MptcpSocket sock fid) token locId = let
--     m0 = Map.empty
--     m1 = Map.insert (fromEnum MPTCP_ATTR_TOKEN) (convertToken token) m0
--     m2 = Map.insert (fromEnum MPTCP_ATTR_LOC_ID) (convertLocId locId) m1
--     -- MPTCP_ATTR_IF_IDX and MPTCP_ATTR_BACKUP should be optional
--     attributes = m2
--     -- intCmd = fromEnum cmd
--     cmd = MPTCP_CMD_REMOVE
--     pkt = getRequestPacket fid cmd False attributes
--   in
--     putStrLn ("Remove subflow " ++ showAttributes attributes)
--       >> query sock pkt


checkIfSocketExists :: MptcpSocket -> MptcpToken  -> IO [GenlPacket NoData]
checkIfSocketExists (MptcpSocket sock fid) token = let
    attributes = Map.insert (fromEnum MPTCP_ATTR_TOKEN) (convertToken token) Map.empty
    pkt = getRequestPacket fid cmd False attributes
    cmd = MPTCP_CMD_EXIST
  in
    putStrLn ("Checking token exists\n" ++ showAttributes attributes ++ showPacket pkt)
      -- >> putStrLn $
      >> query sock pkt

--announceSubflow :: MptcpSocket -> MptcpToken -> IO [GenlPacket NoData]
--announceSubflow (MptcpSocket socket fid) token = let
--    m0 = Map.empty
--    -- TODO I should add the command at some point here
--    m1 = Map.insert MPTCP_ATTR_TOKEN (convertToken token) m0
--    -- the loc id should exist :/
--    m2 = Map.insert MPTCP_ATTR_LOC_ID (convertToken token) m1
--    -- MPTCP_ATTR_IF_IDX and MPTCP_ATTR_BACKUP should be optional
--    --
--    attributes = m1
--    intCmd = fromEnum cmd
--    cmd = MPTCP_CMD_ANNOUNCE
--    pkt = getRequestPacket fid cmd False attributes
--  in
--    query socket pkt


-- TODO here I could helpers from the netlink library
convertLocId :: Word8 -> ByteString
convertLocId val =
  -- putWord8 val
  runPut $ putWord8 val
  -- toByteString val


-- put32 :: Word32 -> ByteString
-- put32 w = runPut (putWord32host w)
convertToken :: Word32 -> ByteString
convertToken val =
  runPut $ putWord32le val

-- need to prepare a request
-- type GenlPacket a = Packet (GenlData a)
-- getResetPacket
resetTheConnection :: MptcpSocket -> Attributes -> IO [GenlPacket NoData]
resetTheConnection (MptcpSocket sock fid) receivedAttributes = let
    m0 = Map.empty
    m1 = Map.insert intCmd (convertToken token) m0
    token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) receivedAttributes
    -- family eAF_INET 
    attributes = m1
    intCmd = fromEnum cmd
    cmd = MPTCP_CMD_REMOVE

    -- getRequestPacket fid cmd dump attrs =
    -- getRequestPacket :: Word16 -> MptcpGenlEvent -> Bool -> Attributes -> GenlPacket NoData
    pkt = getRequestPacket fid cmd False attributes
  in
    -- GenlPacket a with a acting as genlDataData
    -- System.Linux.Netlink query :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO [Packet a]
    query sock pkt

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
        let pkt = createNewSubflow sock token attributes
        return ()
      "d" -> putStrLn "Not implemented"
        -- removeSubflow sock token locId >>= inspectAnswers >> putStrLn "Finished announcing"
      "e" -> putStrLn "check for existence" >>
          checkIfSocketExists sock token >>= inspectAnswers
          -- checkIfSocketExists sock token >>= (dispatchPacket sock)
          -- return ()
      "r" -> putStrLn "Reset the connection" >>
        -- TODO expects token
        -- TODO discard result
          -- resetTheConnection sock token >>= inspectAnswers >>
          putStrLn "Finished resetting"
      _ -> onNewConnection sock attributes
    return ()


-- type GenlPacket a = Packet (GenlData a)
-- overloaded dispatching inspired by System/Linux/Netlink.hs:showPacket
-- dispatchPacket :: MptcpSocket -> Packet -> IO ()
-- dispatchPacket sock (System.Linux.Netlink.ErrorMsg hdr code packet) = do
--   putStrLn "a netlink error happened"
-- dispatchPacket sock (DoneMsg hdr) = do
--   putStrLn "Done Msg"

-- type GenlPacket a = Packet (GenlData a)
-- type MptcpPacket = GenlPacket NoDataMptcp
-- dispatchPacket :: MptcpSocket -> GenlPacket NoData -> IO ()
-- dispatchPacket sock (Packet hdr (GenlData NoData) attributes) = do
--     -- cmd = genlCmd genlHeader
--     putStrLn "Genldata"


dispatchPacket :: MyState -> MptcpPacket -> IO MyState
dispatchPacket oldState (Packet hdr (GenlData genlHeader NoData) attributes) = let
    cmd = genlCmd genlHeader
    (MyState sock conns) = oldState

    -- i suppose token is always available right ?
    token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes
    mptcpConn = Map.lookup token (connections oldState)
  in
    case toEnum (fromIntegral cmd) of
      MPTCP_EVENT_CREATED -> do
        let subflow = subflowFromAttributes attributes
        let newMptcpConn = MptcpConnection [ subflow ]
        putStrLn $ "Connection created !!\n" ++ showAttributes attributes
        -- TODO filter connections via command line settings
        -- onNewConnection sock attributes
        forkIO 
        -- r <- createProces $ startMonitor token
        -- putStrLn $ "Connection created !!\n" ++ show subflow

        -- let newState = oldState { connections = Map.insert token newMptcpConn (connections oldState) }
        putStrLn $ "New state after creation: " ++ show newState
        return newState

      MPTCP_EVENT_ESTABLISHED -> do
        putStrLn "Connection established !"
        return oldState

      -- this might be true for subflow
      MPTCP_EVENT_CLOSED -> do
        -- case mptcpConn of
        --   Nothing -> putStrLn "Not found"
        --   Just
        putStrLn "Connection closed, deleting token "
        let newState = oldState { connections = Map.delete token (connections oldState) }
        putStrLn $ "New state" ++ show newState
        return newState
      MPTCP_EVENT_SUB_ESTABLISHED -> do
        let subflow = subflowFromAttributes attributes
        putStrLn "Subflow established"
        case mptcpConn of
            Nothing -> putStrLn "No connection with this token" >> return oldState
            Just con -> do
                let newCon = con { subflows = subflows con ++ [subflow] }
                let newState = oldState { connections = Map.insert token newCon (connections oldState) }
                return newState
      -- TODO remove
      MPTCP_EVENT_SUB_CLOSED -> putStrLn "Subflow closed" >> return oldState
      MPTCP_CMD_EXIST -> putStrLn "this token exists" >> return oldState
      _ -> putStrLn "undefined event !!" >> return oldState


-- dispatchPacket sock (ErrorMsg err) attributes) = let
dispatchPacket s (DoneMsg err) =
  putStrLn "Done msg" >> return s


dispatchPacket s (ErrorMsg hdr errCode errPacket) = do
  putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n" ++ show errPacket
  return s

showError :: Show a => Packet a -> IO ()
showError (ErrorMsg hdr errCode errPacket) = do
  putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n" ++ show errPacket
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
fromOctetsBE :: [Word8] -> Word32
fromOctetsBE = foldl' accum 0
  where
    accum a o = (a `Bits.shiftL` 8) .|. fromIntegral o
    -- maybe I could use putIArrayOf instead

fromOctetsLE :: [Word8] -> Word32
fromOctetsLE = fromOctetsBE . reverse

inspectIDiagAnswer :: (Packet InetDiagMsg) -> IO ()
inspectIDiagAnswer (Packet hdr cus attrs) = do
   putStrLn $ "Idiag answer" ++ showAttributes attrs
inspectIDiagAnswer p = putStrLn $ "test" ++ (showPacket p)

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

  let mptcpConnections = []
  options <- execParser opts
  logger <- createLogger
  pushLogStr logger (toLogStr "ok")
  putStrLn "dumping important values:"
  putStrLn $ "RESET" ++ show MPTCP_CMD_REMOVE
  putStrLn $ dumpMptcpCommands MPTCP_CMD_UNSPEC
  putStrLn "Creating MPTCP netlink socket..."
  (MptcpSocket sock  fid) <- makeMptcpSocket

  putStr "socket created. MPTCP Family id " >> print fid
  -- putStr "socket created. tcp_metrics Family id " >> print fidMetrics
  -- That's what I should use in fact !! (Word16, [CtrlAttrMcastGroup])
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  -- Each netlink family has a set of 32 multicast groups
  -- mcastMetricGroups <- getMulticastGroups sockMetrics fidMetrics
  mcastMptcpGroups <- getMulticastGroups sock fid
  mapM_ print mcastMptcpGroups

  -- mapM_ (listenToMetricEvents sockMetrics) mcastMetricGroups
  -- mapM_ (listenToEvents (MptcpSocket sock fid)) mcastMptcpGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastMptcpGroups )
  putStrLn "finished"

