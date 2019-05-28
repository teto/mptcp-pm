{-|
Module      : MPTCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
module Net.Mptcp
where

import Generated
import IDiag ()
import Net.IPAddress
import Control.Exception (assert)

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
import Data.Serialize.Put

import Data.Bits ((.|.))

import Data.List (intercalate)
-- import Debug.Trace
import Control.Concurrent (MVar)
import Net.IPv4

-- how can I retreive the word16 without pattern matching ?
data MptcpSocket = MptcpSocket NetlinkSocket Word16
instance Show MptcpSocket where
  show sock = let (MptcpSocket nlSock fid) = sock in ("Mptcp netlink socket: " ++ show fid)

-- isIPv4address
-- |Data to hold subflows information
data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: IPAddress -- ^Source ip
  , dstIp :: IPAddress
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
                -- -> Attributes
                -> [MptcpAttribute]
                -> MptcpPacket
genMptcpRequest fid cmd dump attrs =
  let
    -- The message type/ flag / sequence number / pid  (0 => from the kernel)
    -- https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/netlink.h#L54
    myHeader = Header (fromIntegral fid) (flags .|. fNLM_F_ACK) 0 0
    geheader = GenlHeader word8Cmd mptcpGenlVer
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
    word8Cmd = fromIntegral (fromEnum cmd) :: Word8

    -- TODO run an assert on the list filter
  in
    -- TODO check that MptcpToken is in the list
    Packet myHeader (GenlData geheader NoData) (mptcpListToAttributes attrs)

readToken :: Maybe ByteString -> MptcpToken
readToken maybeVal = case maybeVal of
  Nothing -> error "Missing token"
  Just val -> case ( runGet getWord32host val) of
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
    , family :: Word16 -- Remove ?
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
    MptcpAddr IPAddress |
    MptcpSrcPort Word16 |
    MptcpDstPort Word16 |
    MptcpIntf Word8

type MptcpToken = Word32
type LocId    = Word8
-- type MptcpFamily    = Word16

-- todo remove when possible
toIpv4 :: ByteString -> String
toIpv4 val = Data.List.intercalate "." ( map show (unpack val))

-- taken from System/Linux/Netlink/GeNetlink/Control.hs
-- ctrlAttributesFromAttributes :: Map Int ByteString -> [CtrlAttribute]
-- ctrlAttributesFromAttributes = map getAttribute . toList
-- |Convert the typesafe 'CtrPacket' into a 'CTRLPacket' so it can be sent
-- ctrlPackettoGenl :: CtrlPacket -> CTRLPacket
-- ctrlPackettoGenl (CtrlPacket h g attrs)= Packet h (GenlData g NoData) a
--   where a = fromList $map ctrlAttributesToAttribute attrs

-- mptcpAttributeToTuple :: MptcpAttribute -> (Int, ByteString)
-- mptcpAttributeToTuple (MptcpAttrToken token) = (fromEnum MPTCP_ATTR_TOKEN, runPut $ putWord32host token)
-- mptcpAttributeToTuple (MptcpIntf ifx) = (fromEnum MPTCP_ATTR_IF_IDX, runPut $ putWord8 ifx)
-- mptcpAttributeToTuple (MptcpAddr addr) = (fromEnum MPTCP_ATTR_SADDR4, encodeUtf8 addr)
-- -- (fromEnum family) :: Word8)
-- mptcpAttributeToTuple (SubflowFamily _) = (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord8 eAF_INET)
-- mptcpAttributeToTuple (RemoteLocatorId loc) = (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord8 loc)
-- mptcpAttributeToTuple (LocalLocatorId loc) = (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord8 loc)

-- inspired by netlink cATA :: CtrlAttribute -> (Int, ByteString)
attrToPair :: MptcpAttribute -> (Int, ByteString)
attrToPair (MptcpAttrToken token) = (fromEnum MPTCP_ATTR_TOKEN, runPut $ putWord32host token)
attrToPair (RemoteLocatorId loc) = (fromEnum MPTCP_ATTR_REM_ID, runPut $ putWord8 loc)
attrToPair (LocalLocatorId loc) = (fromEnum MPTCP_ATTR_LOC_ID, runPut $ putWord8 loc)
attrToPair (SubflowFamily fam) = let
        fam8 = (fromIntegral $ fromEnum fam) :: Word8
    in (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord8 fam8)

attrToPair ( MptcpIntf idx) = (fromEnum MPTCP_ATTR_IF_IDX, runPut $ putWord8 idx)
attrToPair ( MptcpAddr addr) = (fromEnum MPTCP_ATTR_SADDR4, encodeUtf8 addr)
attrToPair ( MptcpSrcPort port) = (fromEnum MPTCP_ATTR_SPORT, runPut $ putWord16host port)
attrToPair ( MptcpDstPort port) = (fromEnum MPTCP_ATTR_DPORT, runPut $ putWord16host port)

-- attrToPair _ = error "unsupported"

mptcpListToAttributes :: [MptcpAttribute] -> Attributes
mptcpListToAttributes attrs = Map.fromList $map attrToPair attrs


-- mptcpAttributesToMap :: [MptcpAttribute] -> Attributes
-- mptcpAttributesToMap attrs =
--   Map.fromList $map mptcpAttributeToTuple attrs

subflowFromAttributes :: Attributes -> TcpConnection
subflowFromAttributes attrs =
  let
    -- expects a ByteString
    _srcPort = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_SPORT) attrs)
    -- toIpv4 $ fromJust
    _srcIp =  case (Map.lookup (fromEnum MPTCP_ATTR_SADDR4) attrs) of
      -- TODO toIpv4 ip
      Just ip -> fromJust $ decodeUtf8 ip
      -- assume v6, throws otherwise
      -- Nothing -> fromJust (Map.lookup (fromEnum MPTCP_ATTR_SADDR6) attrs)
      -- TODO user 'error' instead
      Nothing -> error "ipv6 src"
    _dstPort = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_DPORT) attrs)
    _dstIp = case (Map.lookup (fromEnum MPTCP_ATTR_DADDR4) attrs) of
      Just ip -> fromJust $ decodeUtf8 ip
      -- assume v6, throws otherwise
      -- Nothing -> fromJust (Map.lookup (fromEnum MPTCP_ATTR_SADDR6) attrs)
      Nothing -> error "ipv6 dst"
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
    | i == fromEnum MPTCP_ATTR_DADDR4 = MptcpAddr <$> decodeUtf8 val
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
  -- traceShowId
  attrStr = case  ( toEnum (fromIntegral attr)) of
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


checkIfSocketExistsPkt :: Word16 -> [MptcpAttribute]  -> MptcpPacket
checkIfSocketExistsPkt fid attributes =
    genMptcpRequest fid MPTCP_CMD_EXIST True attributes

-- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell?noredirect=1&lq=1
-- attrToPair ( MptcpSrcPort port) = (fromEnum MPTCP_ATTR_SPORT, runPut $ putWord8 loc)
isAttribute :: MptcpAttribute -- ^ to compare with
               -> MptcpAttribute -- ^to compare to
               -> Bool
isAttribute ref toCompare = fst (attrToPair toCompare) == fst (attrToPair ref)

-- need to prepare a request
-- type GenlPacket a = Packet (GenlData a)
-- REQUIRES: LOC_ID / TOKEN
resetConnectionPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
resetConnectionPkt (MptcpSocket sock fid) attrs = let
    cmd = MPTCP_CMD_REMOVE
    tokenAttrs = filter (isAttribute (MptcpAttrToken 0)) attrs
  in
    assert ((length tokenAttrs) > 0) $ genMptcpRequest fid MPTCP_CMD_REMOVE False attrs

-- TODO here I could helpers from the netlink library
convertLocId :: Word8 -> ByteString
convertLocId val =
  runPut $ putWord8 val


-- sport/backup/intf are optional
-- family AF_INET/loc id/remid are not
newSubflowPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
newSubflowPkt (MptcpSocket sock fid) attrs = let
    cmd = MPTCP_CMD_SUB_CREATE
    pkt = genMptcpRequest fid MPTCP_CMD_SUB_CREATE False attrs
  in
    pkt

  -- putStrLn "createNewSubflow TODO"
  -- if (!info->attrs[MPTCP_ATTR_TOKEN] || !info->attrs[MPTCP_ATTR_FAMILY] ||
  --     !info->attrs[MPTCP_ATTR_LOC_ID] || !info->attrs[MPTCP_ATTR_REM_ID])

