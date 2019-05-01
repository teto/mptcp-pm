{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux


This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h

To interact
GENL_ADMIN_PERM
The operation requires the CAP_NET_ADMIN privilege



Helpful links:
- How to use Map https://lotz84.github.io/haskellbyexample/ex/maps
= How to update a single field https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
- get tcp stats via netlink https://www.vividcortex.com/blog/2014/09/22/using-netlink-to-optimize-socket-statistics/
eNETLINK_INET_DIAG

http://kristrev.github.io/2013/07/26/passive-monitoring-of-sockets-on-linux


-}

-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)
import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)

-- For TcpState, FFI generated
import Generated

-- for replicateM
-- import Control.Monad
import Data.Maybe
import Data.Foldable (foldl')
import Data.Bits ((.|.))
import Foreign.C.Types (CInt)
import Foreign.C.Error
import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, Packet(..))
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- for pX and gY
-- import System.Linux.Netlink.Helpers
import System.Log.FastLogger

-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/process-1.6.5.0/System-Process.html
import System.Process
import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word8, Word16, Word32)
import Data.List (intercalate)
import Data.Bits (shiftL, )

-- imported from cereal  hiding (runGet)
import Data.Serialize.Get
import Data.Serialize.Put
import IDiag

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

-- how can I retreive the word16 without pattern matching ?
data MptcpSocket = MptcpSocket NetlinkSocket Word16
instance Show MptcpSocket where
  show sock = let (MptcpSocket nlSock fid) = sock in ("Mptcp netlink socket: " ++ show fid)

data MetricsSocket = MetricsSocket NetlinkSocket Word16

-- inspired by NoData80211
-- data NoDataMptcp = NoDataMptcp deriving (Eq, Show)
-- instance Convertable NoDataMptcp where
--   getPut _ = return ()
--   getGet _ = return NoDataMptcp

iperfHardcodedSrcPort :: Word16
iperfHardcodedSrcPort = 5500

-- isIPv4address
-- rename  or extend subflow
data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: String
  , dstIp :: String
  , srcPort :: Word16
  , dstPort :: Word16
  -- , localId :: Word8
  -- , remoteId :: Word8
  -- add loc id
  -- add TcpMetrics member

-- TODO derive Eq as well
} deriving Show

-- instance Show TcpConnection where
--   show (TcpConnection srcIp dstIP srcPort dstPort) = "Src IP: " ++ srcIp

newtype MptcpConnection = MptcpConnection {
  subflows :: [TcpConnection]
} deriving Show

data MyState = MyState {
  -- Use a map instead to map on token
  socket :: MptcpSocket
  , connections :: Map.Map MptcpToken MptcpConnection
  -- TODO pass the socket as well ?
} deriving Show

-- data GenlData a = GenlData
--     {
--       genlDataHeader :: GenlHeader
--     , genlDataData   :: a
--     } deriving (Eq)
  --
-- type GenlPacket a = Packet (GenlData a)
-- type Mptcp
    --
type MptcpPacket = GenlPacket NoData
-- type MptcpPacket = Packet (GenlData NoDataMptcp)

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
data MptcpAttributes = MptcpAttributes {
    connToken :: Word32
    , localLocatorID :: Maybe Word8
    , remoteLocatorID :: Maybe Word8
    , family :: Word16
    -- |Pointer to the Attributes map used to build this struct. This is purely
    -- |for forward compat, please file a feature report if you have to use this.
    , staSelf       :: Attributes
} deriving (Show, Eq, Read)

data MptcpAttribute =
    MptcpAttrToken MptcpToken |
    -- v4 or v6, AddressFamily is a netlink def
    SubflowFamily AddressFamily |
    -- remote/local ?
    RemoteLocatorId Word8 |
    LocalLocatorId Word8 |
    MptcpAddr ByteString |
    MptcpIntf Word8

type MptcpToken = Word32
type LocId    = Word8
-- type MptcpFamily    = Word16


-- TODO prefix with 'e' for enum
-- Map.lookup (fromEnum attr) m
getAttribute :: MptcpAttr -> Attributes -> Maybe MptcpAttribute
getAttribute attr m
    | attr == MPTCP_ATTR_TOKEN = Nothing
    | otherwise = Nothing

-- getAttribute :: (Int, ByteString) -> CtrlAttribute
-- getAttribute (i, x) = fromMaybe (CTRL_ATTR_UNKNOWN i x) $makeAttribute i x

-- getW16 :: ByteString -> Maybe Word16
-- getW16 x = e2M (runGet g16 x)

-- getW32 :: ByteString -> Maybe Word32
-- getW32 x = e2M (runGet g32 x)

-- "either2Maybe"
-- e2M :: Either a b -> Maybe b
-- e2M (Right x) = Just x
-- e2M _ = Nothing

makeAttribute :: Int -> ByteString -> Maybe MptcpAttribute
makeAttribute i val
    | i == fromEnum MPTCP_ATTR_TOKEN = Just (MptcpAttrToken $ readToken $ Just val)
    | i == fromEnum MPTCP_ATTR_FAMILY = Just (SubflowFamily $ AddressFamily fromByteString val  )
    | i == fromEnum MPTCP_ATTR_DADDR4 = Just (MptcpAddr $ getWord8 )
    | i == fromEnum MPTCP_ATTR_LOC_ID = Just (MptcpAddr $ getWord8 )
    -- | i == fromEnum MPTCP_ATTR_REM_ID = Just (MptcpAddr $ getWord8 )
    -- | i == MPTCP_ATTR_FAMILY = Just (MptcpAttrToken $ runGet getWord32le val)

dumpAttribute :: Int -> ByteString -> String
dumpAttribute attr value = let
  -- enumFromTo ?
  attrStr = case toEnum (fromIntegral attr) of
      MPTCP_ATTR_UNSPEC -> "UNSPECIFIED"
      MPTCP_ATTR_TOKEN -> "token: " ++ show (readToken $ Just value)
      MPTCP_ATTR_FAMILY -> "family: " ++ show value
      MPTCP_ATTR_LOC_ID -> "Locator id: " ++ show (readLocId $ Just value)
      MPTCP_ATTR_REM_ID -> "Remote id: " ++ show value
      MPTCP_ATTR_SADDR4 -> "ipv4.src: " ++ toIpv4 value
      MPTCP_ATTR_SADDR6 -> "ipv6.src: " ++ show value
      MPTCP_ATTR_DADDR4 -> "ipv4.dest: " ++ toIpv4 value
      MPTCP_ATTR_DADDR6 -> "ipv6.dest: " ++ show value
      MPTCP_ATTR_SPORT -> "sport: " ++ show (getPort value)
      MPTCP_ATTR_DPORT -> "dport: " ++ show (getPort value)
      MPTCP_ATTR_BACKUP -> "backup" ++ show value
      MPTCP_ATTR_ERROR -> "Error: " ++ show value
      MPTCP_ATTR_FLAGS -> "Flags: " ++ show value
      MPTCP_ATTR_TIMEOUT -> "timeout: " ++ show value
      MPTCP_ATTR_IF_IDX -> "ifId: " ++ show value
      -- _ -> "unhandled case"
  in
    attrStr


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



tcpMetricsGenlName :: String
tcpMetricsGenlName = "tcp_metrics"
tcpMetricsGenlVer :: Word8
tcpMetricsGenlVer = 1

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


runMptcpNumerics :: IO String
runMptcpNumerics  =
  -- TODO run readProcessWithExitCode instead
  readProcess "seq" ["1", "10"] ""


-- inspectPacket :: GenlPacket NoData -> IO ()
-- inspectPacket = Prelude.putStrLn show
-- inspectPacket packet = Prelude.putStrLn $show packet


-- TODO merge default attributes
-- explain what "dump" does
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

toIpv4 :: ByteString -> String
toIpv4 val = Data.List.intercalate "." ( map show (unpack val))


-- TODO convert port
-- t = BS.pack [ 232, 229 ]
-- Data.ByteString.Conversion.fromByteString t :: Maybe Data.Word.Word16

getPort :: ByteString -> Word16
getPort val =
  -- decode (BSL.fromStrict value) :: Word16
  -- runGet getWord16le (BSL.fromStrict val)
  -- runGet getWord16le val
  fromJust $ fromByteString val
  -- getWord16le val
  -- runGet getWord16le val

readToken :: Maybe ByteString -> MptcpToken
readToken maybeVal = case maybeVal of
  Nothing -> error "Missing token"
  Just val -> case ( runGet getWord32le val) of
    Left err -> error "could not decode"
    Right token -> token

readLocId :: Maybe ByteString -> LocId
readLocId maybeVal = case maybeVal of
  Nothing -> error "Missing locator id"
  Just val -> fromJust $ fromByteString val
  -- runGet getWord8 val




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

subflowFromAttributes :: Attributes -> TcpConnection
subflowFromAttributes attrs =
  let
    -- expects a ByteString
    _srcPort = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_SPORT) attrs)
    -- toIpv4 $ fromJust
    _srcIp =  case (Map.lookup (fromEnum MPTCP_ATTR_SADDR4) attrs) of
      -- TODO toIpv4 ip
      Just ip -> toIpv4 ip
      -- assume v6, throws otherwise
      -- Nothing -> fromJust (Map.lookup (fromEnum MPTCP_ATTR_SADDR6) attrs)
      Nothing -> "ipv6 src"
    _dstPort = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_DPORT) attrs)
    _dstIp = case (Map.lookup (fromEnum MPTCP_ATTR_DADDR4) attrs) of
      Just ip -> toIpv4 ip
      -- assume v6, throws otherwise
      -- Nothing -> fromJust (Map.lookup (fromEnum MPTCP_ATTR_SADDR6) attrs)
      Nothing -> "ipv6 dst"
  in
    TcpConnection _srcIp _dstIp _srcPort _dstPort

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
        onNewConnection sock attributes
        putStrLn $ "Connection created !!\n" ++ show subflow
        -- showAttributes attributes

        -- syntax seen at https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
        let newState = oldState { connections = Map.insert token newMptcpConn (connections oldState) }
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


--
inspectResult :: MyState -> Either String MptcpPacket -> IO()
inspectResult myState result =  case result of
    Left ex -> putStrLn $ "An error in parsing happened" ++ show ex
    Right myPack -> dispatchPacket myState myPack >> putStrLn "Valid packet"

-- CtrlAttrMcastGroup
-- copied from utils/GenlInfo.hs
doDumpLoop :: MyState -> IO MyState
doDumpLoop myState = do
    let (MptcpSocket simpleSock fid) = socket myState
    putStrLn "doDumpLoop"
    -- TODO do less filtering here ?
    -- myPack <- trace "recvOne" (recvOne simpleSock :: IO [GenlPacket NoData])

    -- inspired by https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
    -- to work around "user error (too few bytes       From: demandInput     )"
    results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]

    -- TODO retrieve packets
    mapM_ (inspectResult myState) results

    -- ca me retourne un tas de paquet en fait ?
    -- For a version that ignores the results see mapM_.
    -- _ <- mapM_ (dispatchPacket mptcpSocket) myPack
    -- _ <- mapM inspectPacket  pack
    newState <- doDumpLoop myState
    return newState

-- regarder dans query/joinMulticastGroup/recvOne
-- doDumpLoop / dumpGeneric
-- why doesnt it complain that
listenToEvents :: MptcpSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents (MptcpSocket sock fid) my_group = do
  -- joinMulticastGroup  returns IO ()
  -- TODO should check it works correctly !
  joinMulticastGroup sock (grpId my_group)
  putStrLn $ "Joined grp " ++ grpName my_group
  _ <- doDumpLoop globalState
  putStrLn "TOTO"
  where
    mptcpSocket = MptcpSocket sock fid
    globalState = MyState mptcpSocket Map.empty

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

-- TODO rename to a TCP one ? SockDiagRequest
data SockDiagRequest = SockDiagRequest {
  sdiag_family :: Word8 -- ^AF_INET6 or AF_INET
-- It should be set to the appropriate IPPROTO_* constant for AF_INET and AF_INET6, and to 0 otherwise.
  , sdiag_protocol :: Word8
  -- IPv4/v6 specific structure
  -- InetDiagInfo
  , idiag_ext :: IDiagExt -- ^query extended info
  , req_pad :: Word8        -- ^ padding for backwards compatibility with v1
  , idiag_states :: Word32 -- ^States to dump (based on TcpDump)
    -- struct inet_diag_sockid id;
  , diag_sockid :: InetDiagSockId
} deriving (Eq, Show)

{- |Typeclase used by the system. Basically 'Storable' for 'Get' and 'Put'
getGet Returns a 'Get' function for the convertable.
The MessageType is passed so that the function can parse different data structures
based on the message type.
-}
-- class Convertable a where
--   getGet :: MessageType -> Get a -- ^get a 'Get' function for the static data
--   getPut :: a -> Put -- ^get a 'Put' function for the static data
instance Convertable SockDiagRequest where
  getPut = putSockDiagRequestHeader
  -- MessageType
  getGet _ = getSockDiagRequestHeader

-- |'Get' function for 'GenlHeader'
-- applicative style Trade <$> getWord32le <*> getWord32le <*> getWord16le
getSockDiagRequestHeader :: Get SockDiagRequest
getSockDiagRequestHeader = do
    addressFamily <- getWord8 -- AF_INET for instance
    protocol <- getWord8
    -- TODO convert it ?
    -- extended <- getWord8
    -- TODO la ca foire
    -- let extended = InetDiagNone
    extended <- getWord32host
    _pad <- getWord8
    states <- getWord32host
    _sockid <- getInetDiagSockid
    return $ SockDiagRequest addressFamily protocol (toEnum (fromIntegral extended) :: IDiagExt) _pad states _sockid

-- |'Put' function for 'GenlHeader'
putSockDiagRequestHeader :: SockDiagRequest -> Put
putSockDiagRequestHeader hdr = do
  putWord8 $ sdiag_family hdr
  putWord8 $ sdiag_protocol hdr
  -- extended
  putWord8 $ fromIntegral $ fromEnum $ idiag_ext hdr
  putWord8 $ req_pad hdr
  -- TODO check endianness
  putWord32be $ idiag_states hdr
  putInetDiagSockid $ diag_sockid hdr

-- where struct inet_diag_sockid is defined as follows:
--     struct inet_diag_sockid {
--         __be16  idiag_sport;
--         __be16  idiag_dport;
--         __be32  idiag_src[4];
--         __be32  idiag_dst[4];
--         __u32   idiag_if;
--         __u32   idiag_cookie[2];
--     };

-- data IDiagFamily = AF_INET | AF_INET6

-- this is inspired
--  All values are in network byte order.
--  idiag_sport prefix them


-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6

-- inspired by http://man7.org/linux/man-pages/man7/sock_diag.7.html

-- 
-- instance Bits TcpState where

-- Sends a SockDiagRequest
-- expects INetDiag 
queryTcpStats :: NetlinkSocket -> IO ()
queryTcpStats sock = let
  req = Packet
  -- Mesge type / flags /seqNum /pid
  -- or DUMP_INTR or DUMP_FILTERED ?
  -- NLM_F_DUMP flag set, it means that a list of sockets is being
  -- requested; otherwise it is a query about an individual socket.
  -- flags sont pas bons
  -- NLM_F_ROOT|NLM_F_MATCH|NLM_F_REQUEST,\
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
  -- TCP states taken from include/net/tcp_states.h TCP_LISTEN,
  -- idiag_states = TCPF_ALL & ~(TCPF_SYN_RECV | TCPF_TIME_WAIT | TCPF_CLOSE);
  stateFilter = 0 :: Word32
  -- stateFilter = fromIntegral (fromEnum TcpListen) :: Word32
  -- requestedInfo = InetDiagInfo
  requestedInfo = InetDiagNone   -- <=> 0
  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo 0 (stateFilter) diag_req
      -- NLC.eRTM_GETLINK (NLC.fNLM_F_ROOT .|. NLC.fNLM_F_MATCH .|. NLC.fNLM_F_REQUEST) 0 0)
  -- packet header Custom Attributes
  -- pkt = Packet
  in
    sendPacket sock $ Packet hdr custom Map.empty

-- for big endian
-- https://mail.haskell.org/pipermail/beginners/2010-October/005571.html
-- foldl' :: (b -> a -> b) -> b -> Maybe a -> b
fromOctetsBE :: [Word8] -> Word32
fromOctetsBE = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o
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



-- Packet of 
inspectIdiagAnswers :: [Packet InetDiagMsg] -> IO ()
inspectIdiagAnswers packets = do
  putStrLn "Start inspecting IDIAG answers"
  mapM_ inspectIDiagAnswer packets
  putStrLn "Finished inspecting answers"

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do

  -- super nice tutorial on optparse applicative:
  -- https://github.com/pcapriotti/optparse-applicative#regular-options
  let mptcpConnections = []
  options <- execParser opts
  logger <- createLogger
  pushLogStr logger (toLogStr "ok")
  putStrLn "dumping important values:"
  -- putStrLn $ "buffer size " ++ show bufferSize
  putStrLn $ "RESET" ++ show MPTCP_CMD_REMOVE
  putStrLn $ dumpMptcpCommands MPTCP_CMD_UNSPEC
  putStrLn "Creating MPTCP netlink socket..."
  (MptcpSocket sock  fid) <- makeMptcpSocket
  -- (sockMetrics, fidMetrics) <- makeMetricsSocket
  --
  putStrLn "Creating metrics netlink socket..."
  sockMetrics <- makeMetricsSocket


  -- sendPacket
  queryTcpStats sockMetrics >> putStrLn "Sent the TCP SS request"

  -- exported from my own version !!
  recvMulti sockMetrics >>= inspectIdiagAnswers
  -- recvOne sockMetrics >>= inspectIdiagAnswers

-- NetlinkSocket
  putStr "socket created. MPTCP Family id " >> print fid
  -- putStr "socket created. tcp_metrics Family id " >> print fidMetrics
  -- That's what I should use in fact !! (Word16, [CtrlAttrMcastGroup])
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  -- Each netlink family has a set of 32 multicast groups
  -- mcastMetricGroups <- getMulticastGroups sockMetrics fidMetrics
  mcastMptcpGroups <- getMulticastGroups sock fid
  mapM_ print mcastMptcpGroups
  -- mapM_ print mcastMetricGroups

  -- mapM_ (listenToMetricEvents sockMetrics) mcastMetricGroups
  -- mapM_ (listenToEvents (MptcpSocket sock fid)) mcastMptcpGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastMptcpGroups )
  putStrLn "finished"

    -- let flags   = foldr (.|.) 0 [fNLM_F_REQUEST]
    --     header  = Header eRTM_GETLINK flags 42 0
    --     message = LinkMsg 0 2 0
    --     attrs   = empty
    -- iface <- queryOne sock (Packet header message attrs)
    -- print (packetMessage iface)
    -- let attrs = packetAttributes iface
    -- print $ getLinkAddress attrs
    -- print $ getLinkBroadcast attr

-- dumpNumeric :: ByteString -> IO ()
-- dumpNumeric b = print $ unpack b
