{-|
Module      : Net.Mptcp
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

OverloadedStrings allows Aeson to convert
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Net.Mptcp (
  -- * Types
  MptcpConnection (..)
  , MptcpPacket
  , MptcpSocket (..)
  , MptcpToken
  , dumpAttribute
  , mptcpConnAddSubflow
  , mptcpConnRemoveSubflow
  , newSubflowPkt
  , capCwndPkt
  , subflowFromAttributes
  , readToken
  , remoteIdFromAttributes
)
where

import Net.SockDiag ()
import Control.Exception (assert)

import Data.Word (Word8, Word16, Word32)
import qualified Data.Map as Map
import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, Packet(..))
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.GeNetlink.Control
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)

import Data.Serialize.Get
import Data.Serialize.Put

import Data.Bits ((.|.))

import Data.List ()
import Debug.Trace
import Control.Concurrent ()
import Net.IP
import Net.Tcp
import Net.IPAddress
import Net.IPv4
import Net.IPv6
import Net.Mptcp.Constants
import qualified Data.Set as Set
import Data.Aeson
import GHC.Generics
import Control.Monad.Trans.State ()

data MptcpSocket = MptcpSocket NetlinkSocket Word16
instance Show MptcpSocket where
  show sock = let (MptcpSocket _ fid) = sock in ("Mptcp netlink socket: " ++ show fid)

type MptcpPacket = GenlPacket NoData

-- |Same as SockDiagMetrics
-- data SubflowWithMetrics = SubflowWithMetrics {
--   subflowSubflow :: TcpConnection
--     -- for now let's retain DiagTcpInfo  only
--   , metrics :: [SockDiagExtension]
-- }

-- |Holds MPTCP level information
data MptcpConnection = MptcpConnection {
  connectionToken :: MptcpToken
  -- use SubflowWithMetrics instead ?!
  -- , subflows :: Set.Set [TcpConnection]
  , subflows :: Set.Set TcpConnection
  , localIds :: Set.Set Word8  -- ^ Announced addresses
  , remoteIds :: Set.Set Word8   -- ^ Announced addresses

  -- Might be reworked/moved in an Enriched/Tracker structure afterwards
  , get_caps_prog :: Maybe FilePath
} deriving (Show, Generic)

-- | Remote port
data RemoteId = RemoteId {

  remoteAddress :: IP
  , remotePort :: Word16
}



remoteIdFromAttributes :: Attributes -> RemoteId
remoteIdFromAttributes attrs = let
    (SubflowDestPort dport) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_DPORT attrs
    -- (SubflowFamily _) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_FAMILY attrs
    SubflowDestAddress destIp = ipFromAttributes False attrs

    -- (SubflowDestPort dport) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_DPORT attrs
  in
    RemoteId destIp dport

-- we don't really care
instance FromJSON MptcpConnection

-- | export to the format expected by mptcpnumerics
-- could be automatically generated ?
-- toJSON :: MptcpConnection -> Value
instance ToJSON MptcpConnection where
  toJSON mptcpConn = object
    [ "name" .= toJSON (show $ connectionToken mptcpConn)
    , "sender" .= object [
          -- TODO here we could read from sysctl ? or use another SockDiagExtension
          "snd_buffer" .= toJSON (40 :: Int)
          , "capabilities" .= object []
        ]
    , "capabilities" .= object ([])
    -- TODO generated somewhere else
    -- , "subflows" .= object ([])
    ]


-- |Adds a subflow to the connection
-- TODO compose with mptcpConnAddLocalId
mptcpConnAddSubflow :: MptcpConnection -> TcpConnection -> MptcpConnection
mptcpConnAddSubflow mptcpConn sf =
  -- trace ("Adding subflow" ++ show sf)
    mptcpConnAddLocalId
        (mptcpConnAddRemoteId
            (mptcpConn { subflows = Set.insert sf (subflows mptcpConn) })
            (remoteId sf)
        )
        (localId sf)

    -- , localIds = Set.insert (localId sf) (localIds mptcpConn)
    -- , remoteIds = Set.insert (remoteId sf) (remoteIds mptcpConn)
  -- }


-- |Add local id
mptcpConnAddLocalId :: MptcpConnection
                       -> Word8 -- ^Local id to add
                       -> MptcpConnection
mptcpConnAddLocalId con locId = con { localIds = Set.insert (locId) (localIds con) }


-- |Add remote id
mptcpConnAddRemoteId :: MptcpConnection
                       -> Word8 -- ^Remote id to add
                       -> MptcpConnection
mptcpConnAddRemoteId con remId = con { localIds = Set.insert (remId) (remoteIds con) }

-- |Remove subflow from an MPTCP connection
mptcpConnRemoveSubflow :: MptcpConnection -> TcpConnection -> MptcpConnection
mptcpConnRemoveSubflow con sf = con {
  subflows = Set.delete sf (subflows con)
  -- TODO remove associated local/remote Id ?
}


getPort :: ByteString -> Word16
getPort val =
  case (runGet getWord16host val) of
    Left _ -> 0
    Right port -> port


--
-- The message type/ flag / sequence number / pid  (0 => from the kernel)
-- https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/netlink.h#L54
fixHeader :: MptcpSocket -> Bool -> MptcpPacket -> MptcpPacket
fixHeader _ dump pkt = let
    myHeader = Header 0 (flags .|. fNLM_F_ACK) 0 0
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
  in
    pkt { packetHeader = myHeader }


{-|
  Generates an Mptcp netlink request
TODO we could fake the Word16/Flag and 
-}
genMptcpRequest :: Word16 -- ^the family id
                -> MptcpGenlEvent -- ^The MPTCP command
                -> Bool           -- ^Dump answer (returns EOPNOTSUPP if not possible)
                -- -> Attributes
                -> [MptcpAttribute]
                -> MptcpPacket
genMptcpRequest fid cmd dump attrs =
  let
    myHeader = Header (fromIntegral fid) (flags .|. fNLM_F_ACK) 0 0
    geheader = GenlHeader word8Cmd mptcpGenlVer
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
    word8Cmd = fromIntegral (fromEnum cmd) :: Word8

    pkt = Packet myHeader (GenlData geheader NoData) (mptcpListToAttributes attrs)
    -- TODO run an assert on the list filter
    hasTokenAttr = Prelude.any (isAttribute (MptcpAttrToken 0)) attrs
  in
    assert hasTokenAttr pkt

-- | TODO change / return Either 
readToken :: ByteString -> Either String MptcpToken
readToken val = runGet getWord32host val

-- LocId => Word8
readLocId :: Maybe ByteString -> LocId
readLocId maybeVal = case maybeVal of
  Nothing -> error "Missing locator id"
  Just val -> case runGet getWord8 val of
    -- TODO generate an error here !
    Left _ -> error "Could not get locId !!"
    Right locId -> locId
  -- runGet getWord8 val

-- doDumpLoop :: MyState -> IO MyState
-- doDumpLoop myState = do
--     let (MptcpSocket simpleSock fid) = socket myState
--     results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]
--     -- TODO retrieve packets
--     mapM_ (inspectResult myState) results
--     newState <- doDumpLoop myState
--     return newState


-- data MptcpAttributes = MptcpAttributes {
--     connToken :: Word32
--     , localLocatorID :: Maybe Word8
--     , remoteLocatorID :: Maybe Word8
--     , family :: Word16 -- Remove ?
--     -- |Pointer to the Attributes map used to build this struct. This is purely
--     -- |for forward compat, please file a feature report if you have to use this.
--     , staSelf       :: Attributes
-- } deriving (Show, Eq, Read)

-- Wouldn't it be easier to work with ?
-- data MptcpEvent = NewConnection {
-- }


-- |Represents every possible setting sent/received on the netlink channel
data MptcpAttribute =
    MptcpAttrToken MptcpToken |
    -- v4 or v6, AddressFamily is a netlink def
    SubflowFamily AddressFamily | -- ^ should be Word16 too
    -- remote/local ?
    RemoteLocatorId Word8 |
    LocalLocatorId Word8 |
    SubflowSourceAddress IP |
    SubflowDestAddress IP |
    SubflowSourcePort Word16 |
    SubflowDestPort Word16 |
    SubflowMaxCwnd Word32 |
    SubflowBackup Word8 |
    SubflowInterface Word32
    deriving (Show, Eq)

type MptcpToken = Word32
type LocId    = Word8

-- inspired by netlink cATA :: CtrlAttribute -> (Int, ByteString)
attrToPair :: MptcpAttribute -> (Int, ByteString)
attrToPair (MptcpAttrToken token) = (fromEnum MPTCP_ATTR_TOKEN, runPut $ putWord32host token)
attrToPair (RemoteLocatorId loc) = (fromEnum MPTCP_ATTR_REM_ID, runPut $ putWord8 loc)
attrToPair (LocalLocatorId loc) = (fromEnum MPTCP_ATTR_LOC_ID, runPut $ putWord8 loc)
attrToPair (SubflowFamily fam) = let
        fam8 = (fromIntegral $ fromEnum fam) :: Word16
    in (fromEnum MPTCP_ATTR_FAMILY, runPut $ putWord16host fam8)

attrToPair ( SubflowInterface idx) = (fromEnum MPTCP_ATTR_IF_IDX, runPut $ putWord32host idx)
attrToPair ( SubflowSourcePort port) = (fromEnum MPTCP_ATTR_SPORT, runPut $ putWord16host port)
attrToPair ( SubflowDestPort port) = (fromEnum MPTCP_ATTR_DPORT, runPut $ putWord16host port)
attrToPair ( SubflowMaxCwnd limit) = (fromEnum MPTCP_ATTR_CWND, runPut $ putWord32host limit)
attrToPair ( SubflowBackup prio) = (fromEnum MPTCP_ATTR_BACKUP, runPut $ putWord8 prio)
-- TODO should depend on the ip putWord32be w32
attrToPair ( SubflowSourceAddress addr) =
  case_ (genV4SubflowAddress MPTCP_ATTR_SADDR4) (genV6SubflowAddress MPTCP_ATTR_SADDR6) addr
attrToPair ( SubflowDestAddress addr) =
  case_ (genV4SubflowAddress MPTCP_ATTR_DADDR4) (genV6SubflowAddress MPTCP_ATTR_DADDR6) addr


genV4SubflowAddress :: MptcpAttr -> IPv4 -> (Int, ByteString)
genV4SubflowAddress attr ip = (fromEnum attr, runPut $ putWord32be w32)
  where
    w32 = getIPv4 ip

genV6SubflowAddress :: MptcpAttr -> IPv6 -> (Int, ByteString)
genV6SubflowAddress _addr = undefined

mptcpListToAttributes :: [MptcpAttribute] -> Attributes
mptcpListToAttributes attrs = Map.fromList $Prelude.map attrToPair attrs


-- |Retreive IP
-- TODO could check/use addressfamily as well
ipFromAttributes :: Bool  -- ^True if source
                    -> Attributes -> MptcpAttribute
ipFromAttributes True attrs =
    case makeAttributeFromMaybe MPTCP_ATTR_SADDR4 attrs of
      Just ip -> ip
      Nothing -> case makeAttributeFromMaybe MPTCP_ATTR_SADDR6 attrs of
        Just ip -> ip
        Nothing -> error "could not get the src IP"

ipFromAttributes False attrs =
    case makeAttributeFromMaybe MPTCP_ATTR_DADDR4 attrs of
      Just ip -> ip
      Nothing -> case makeAttributeFromMaybe MPTCP_ATTR_DADDR6 attrs of
        Just ip -> ip
        Nothing -> error "could not get dest IP"

-- mptcpAttributesToMap :: [MptcpAttribute] -> Attributes
-- mptcpAttributesToMap attrs =
--   Map.fromList $map mptcpAttributeToTuple attrs

-- |Converts / should be a maybe ?
-- TODO simplify
subflowFromAttributes :: Attributes -> TcpConnection
subflowFromAttributes attrs =
  let
    -- expects a ByteString
    SubflowSourcePort sport = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_SPORT attrs
    SubflowDestPort dport = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_DPORT attrs
    SubflowSourceAddress _srcIp =  ipFromAttributes True attrs
    SubflowDestAddress _dstIp = ipFromAttributes False attrs
    LocalLocatorId lid = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_LOC_ID attrs
    RemoteLocatorId rid = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_REM_ID attrs
    SubflowInterface intfId = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_IF_IDX attrs
    -- sfFamily = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_FAMILY) attrs)
    prio = Nothing   -- (SubflowPriority N)
  in
    -- TODO fix sfFamily
    TcpConnection _srcIp _dstIp sport dport prio lid rid (Just intfId)


-- TODO prefix with 'e' for enum
-- Map.lookup (fromEnum attr) m
-- getAttribute :: MptcpAttr -> Attributes -> Maybe MptcpAttribute
-- getAttribute attr m
--     | attr == MPTCP_ATTR_TOKEN = Nothing
--     | otherwise = Nothing

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

convertAttributesIntoMap :: Attributes -> Map.Map MptcpAttr MptcpAttribute
convertAttributesIntoMap attrs = let
      customFn k val = fromJust (makeAttribute k val)
      newMap = Map.mapWithKey (customFn) attrs
  in
      Map.mapKeys (toEnum) newMap

-- TODO rename fromMap
makeAttributeFromMaybe :: MptcpAttr -> Attributes -> Maybe MptcpAttribute
makeAttributeFromMaybe attrType attrs =
  let res = Map.lookup (fromEnum attrType) attrs in
  case res of
    Nothing -> error $ "Could not build attr " ++ show attrType
    Just bytestring -> makeAttribute (fromEnum attrType) bytestring

-- | Builds an MptcpAttribute from
makeAttribute :: Int -- ^ MPTCP_ATTR_TOKEN value
                  -> ByteString
                  -> Maybe MptcpAttribute
makeAttribute i val =
  case toEnum i of
    MPTCP_ATTR_TOKEN ->
      case readToken val of 
        Left err -> error "could not decode"
        Right mptcpToken -> Just $ MptcpAttrToken mptcpToken

    -- TODO fix
    MPTCP_ATTR_FAMILY ->
        case runGet getWord16host val of
          -- assert it's eAF_INET or eAF_INET6
          Right x -> Just $ SubflowFamily (toEnum ( fromIntegral x :: Int))
          _ -> Nothing
    MPTCP_ATTR_SADDR4 -> SubflowSourceAddress <$> fromIPv4 <$> e2M ( getIPv4FromByteString val)
    MPTCP_ATTR_DADDR4 -> SubflowDestAddress <$> fromIPv4 <$> e2M (getIPv4FromByteString val)
    MPTCP_ATTR_SADDR6 -> SubflowSourceAddress <$> fromIPv6 <$> e2M (getIPv6FromByteString val)
    MPTCP_ATTR_DADDR6 -> SubflowDestAddress <$> fromIPv6 <$> e2M (getIPv6FromByteString val)
    MPTCP_ATTR_SPORT -> SubflowSourcePort <$> port where port = e2M $ runGet getWord16host val
    MPTCP_ATTR_DPORT -> SubflowDestPort <$> port where port = e2M $ runGet getWord16host val
    MPTCP_ATTR_LOC_ID -> Just (LocalLocatorId $ readLocId $ Just val )
    MPTCP_ATTR_REM_ID -> Just (RemoteLocatorId $ readLocId $ Just val )
    MPTCP_ATTR_IF_IDX -> trace ("if_idx: " ++ show val) (
             case runGet getWord32be val of
                Right x -> Just $ SubflowInterface x
                _ -> Nothing)
    -- backup is u8
    MPTCP_ATTR_BACKUP -> Just (SubflowBackup $ readLocId $ Just val )
    MPTCP_ATTR_ERROR -> trace "makeAttribute ERROR" Nothing
    MPTCP_ATTR_TIMEOUT -> undefined
    MPTCP_ATTR_CWND -> undefined
    MPTCP_ATTR_FLAGS -> trace "makeAttribute ATTR_FLAGS" Nothing
    MPTCP_ATTR_UNSPEC -> undefined


dumpAttribute :: Int -> ByteString -> String
dumpAttribute attrId value =
  show $ makeAttribute attrId value

checkIfSocketExistsPkt :: Word16 -> [MptcpAttribute]  -> MptcpPacket
checkIfSocketExistsPkt fid attributes =
    genMptcpRequest fid MPTCP_CMD_EXIST True attributes

-- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell?noredirect=1&lq=1
-- attrToPair ( SubflowSourcePort port) = (fromEnum MPTCP_ATTR_SPORT, runPut $ putWord8 loc)
isAttribute :: MptcpAttribute -- ^ to compare with
               -> MptcpAttribute -- ^to compare to
               -> Bool
isAttribute ref toCompare = fst (attrToPair toCompare) == fst (attrToPair ref)

-- create a fake LocalLocatorId
hasLocAddr :: [MptcpAttribute] -> Bool
hasLocAddr attrs = Prelude.any (isAttribute (LocalLocatorId 0)) attrs

hasFamily :: [MptcpAttribute] -> Bool
hasFamily = Prelude.any (isAttribute (SubflowFamily eAF_INET))

-- need to prepare a request
-- type GenlPacket a = Packet (GenlData a)
-- REQUIRES: LOC_ID / TOKEN
-- TODO pass TcpConnection 
resetConnectionPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
resetConnectionPkt (MptcpSocket _sock fid) attrs = let
    _cmd = MPTCP_CMD_REMOVE
  in
    assert (hasLocAddr attrs) $ genMptcpRequest fid MPTCP_CMD_REMOVE False attrs


-- pass token ?
subflowAttrs :: TcpConnection -> [MptcpAttribute]
subflowAttrs con = [
    LocalLocatorId $ localId con
    , RemoteLocatorId $ remoteId con
    , SubflowFamily $ getAddressFamily (dstIp con)
    , SubflowDestAddress $ dstIp con
    , SubflowDestPort $ dstPort con
    -- should fail if doesn't exist
    , SubflowInterface $ fromJust $ subflowInterface con
    -- https://github.com/multipath-tcp/mptcp/issues/338
    , SubflowSourceAddress $ srcIp con
    , SubflowSourcePort $ srcPort con
  ]

-- |Generate a request to create a new subflow
capCwndPkt :: MptcpSocket -> MptcpConnection
              -> Word32  -- ^Limit to apply to congestion window
              -> TcpConnection -> MptcpPacket
capCwndPkt (MptcpSocket _ fid) mptcpCon limit sf =
    assert (hasFamily attrs) pkt
    where
        oldPkt = genMptcpRequest fid MPTCP_CMD_SND_CLAMP_WINDOW False attrs
        pkt = oldPkt { packetHeader = (packetHeader oldPkt) { messagePID = 42 } }
        attrs = connectionAttrs mptcpCon
              ++ [ SubflowMaxCwnd limit ]
              ++ subflowAttrs sf


connectionAttrs :: MptcpConnection -> [MptcpAttribute]
connectionAttrs con = [ MptcpAttrToken $ connectionToken con ]

-- sport/backup/intf are optional
newSubflowPkt :: MptcpSocket -> MptcpConnection -> TcpConnection -> MptcpPacket
newSubflowPkt (MptcpSocket _ fid) mptcpCon sf = let
    _cmd = MPTCP_CMD_SUB_CREATE
    attrs = connectionAttrs mptcpCon ++ subflowAttrs sf
    pkt = genMptcpRequest fid MPTCP_CMD_SUB_CREATE False attrs
  in
    assert (hasFamily attrs) pkt

