{-|
Module      : MPTCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
module Mptcp
where

import Generated
import IDiag ()

import Data.Word (Word8, Word16, Word32)
import qualified Data.Map as Map
import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, Packet(..))
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.GeNetlink.Control
import Data.ByteString (ByteString, unpack)
import Data.Maybe (fromJust)
import Data.ByteString.Conversion (fromByteString)

import Data.Serialize.Get
-- import Data.Serialize.Put

import Data.Bits ((.|.))

import Data.List (intercalate)
import Debug.Trace
import Control.Concurrent (MVar)

-- how can I retreive the word16 without pattern matching ?
data MptcpSocket = MptcpSocket NetlinkSocket Word16
instance Show MptcpSocket where
  show sock = let (MptcpSocket nlSock fid) = sock in ("Mptcp netlink socket: " ++ show fid)

-- isIPv4address
-- |Data to hold subflows information
data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: String -- ^Source ip
  , dstIp :: String
  , srcPort :: Word16
  , dstPort :: Word16
  -- , localId :: Word8
  -- , remoteId :: Word8
  -- add loc id
  -- add TcpMetrics member

-- TODO derive Eq as well
} deriving Show


data MyState = MyState {
  socket :: MptcpSocket -- ^Socket
  -- ThreadId/MVar
  , connections :: Map.Map MptcpToken (MVar MptcpConnection)
}
-- deriving Show

type MptcpPacket = GenlPacket NoData

-- |Data to hold MPTCP level information
data MptcpConnection = MptcpConnection {
  connectionToken :: MptcpToken,
  subflows :: [TcpConnection]
} deriving Show

getPort :: ByteString -> Word16
getPort val =
  -- runGet getWord16le (BSL.fromStrict val)
  case fromByteString val of
    Nothing -> 0
    Just port -> port

-- TODO merge default attributes
-- todo pass a list of (Int, Bytestring) and build the map with fromList ?
{-|
  Generates an Mptcp netlink request
-}
genMptcpRequest :: Word16 -- ^ the family id
                -> MptcpGenlEvent -- ^The MPTCP command
                -> Bool           -- ^Dump answer
                -> Attributes
                -> MptcpPacket
genMptcpRequest fid cmd dump attrs =
  let
    -- The message type/ flag / sequence number / pid  (0 => from the kernel)
    -- https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/netlink.h#L54
    myHeader = Header (fromIntegral fid) (flags .|. fNLM_F_ACK) 0 0
    geheader = GenlHeader word8Cmd mptcpGenlVer
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
    word8Cmd = fromIntegral (fromEnum cmd) :: Word8
  in
    Packet myHeader (GenlData geheader NoData) attrs


readToken :: Maybe ByteString -> MptcpToken
readToken maybeVal = case maybeVal of
  Nothing -> error "Missing token"
  Just val -> case ( runGet getWord32le val) of
    Left err -> error "could not decode"
    Right mptcpToken -> mptcpToken

-- LocId => Word8
readLocId :: Maybe ByteString -> LocId
readLocId maybeVal = case maybeVal of
  Nothing -> error "Missing locator id"
  Just val -> case fromByteString val of
    -- TODO generate an error here !
    Nothing -> 0
    Just locId -> locId
  -- runGet getWord8 val

inspectResult :: MyState -> Either String MptcpPacket -> IO()
inspectResult myState result =  case result of
    Left ex -> putStrLn $ "An error in parsing happened" ++ show ex
    -- Right myPack -> dispatchPacket myState myPack >> putStrLn "Valid packet"
    Right myPack ->  putStrLn "inspect result Valid packet"


-- doDumpLoop :: MyState -> IO MyState
-- doDumpLoop myState = do
--     let (MptcpSocket simpleSock fid) = socket myState
--     results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]
--     -- TODO retrieve packets
--     mapM_ (inspectResult myState) results
--     newState <- doDumpLoop myState
--     return newState


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

toIpv4 :: ByteString -> String
toIpv4 val = Data.List.intercalate "." ( map show (unpack val))

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
e2M :: Either a b -> Maybe b
e2M (Right x) = Just x
e2M _ = Nothing

-- TODO use e2m
makeAttribute :: Int -> ByteString -> Maybe MptcpAttribute
makeAttribute i val
    | i == fromEnum MPTCP_ATTR_TOKEN = Just (MptcpAttrToken $ readToken $ Just val)
    -- TODO fix $ Just fromByteString val  )
    | i == fromEnum MPTCP_ATTR_FAMILY = Just (SubflowFamily $ eAF_INET)
    -- this is a bytestring
    | i == fromEnum MPTCP_ATTR_DADDR4 = Just (MptcpAddr $ val )
    -- | i == fromEnum MPTCP_ATTR_LOC_ID = Just (RemoteLocatorId $ val )
    -- | i == fromEnum MPTCP_ATTR_LOC_ID = Just (MptcpAddr $ val )
    | i == fromEnum MPTCP_ATTR_IF_IDX =
             case runGet getWord8 val of
                Right x -> Just $ MptcpIntf x
                _ -> Nothing
    | otherwise = error "Unsupported attribute"
        -- Just (MptcpIntf $ fromMaybe $ e2M () )

    -- | i == fromEnum MPTCP_ATTR_REM_ID = Just (MptcpAddr $ getWord8 )
    -- | i == MPTCP_ATTR_FAMILY = Just (MptcpAttrToken $ runGet getWord32le val)

dumpAttribute :: Int -> ByteString -> String
dumpAttribute attr value = let
  -- enumFromTo ?
  -- traceId $ toEnum (fromIntegral attr)
  attrStr = case traceShowId ( toEnum (fromIntegral attr)) of
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
