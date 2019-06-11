{-|
Module      : MPTCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE DeriveGeneric #-}
module Net.Mptcp
where

import Generated
import IDiag ()
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
import Net.IPAddress
import Net.IPv4
import Net.IPv6
-- in package Unique-0.4.7.6
-- import Data.List.Unique
import Data.Aeson
import GHC.Generics
-- import Data.Default

-- how can I retreive the word16 without pattern matching ?
data MptcpSocket = MptcpSocket NetlinkSocket Word16
instance Show MptcpSocket where
  show sock = let (MptcpSocket nlSock fid) = sock in ("Mptcp netlink socket: " ++ show fid)

-- isIPv4address
-- |Data to hold subflows information
-- use http://hackage.haskell.org/package/data-default-0.5.3/docs/Data-Default.html
-- data default to provide default values
data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: IP -- ^Source ip
  , dstIp :: IP -- ^Destination ip
  , srcPort :: Word16  -- ^ Source port
  , dstPort :: Word16  -- ^Destination port
  , priority :: Maybe Word8 -- ^subflow priority
  , localId :: Word8  -- ^ Convert to AddressFamily
  , remoteId :: Word8
  , inetFamily :: Word16
  , subflowInterface :: Maybe Word32 -- ^Interface of Maybe ?
  -- add TcpMetrics member

} deriving (Show, Generic)


instance FromJSON TcpConnection
instance ToJSON TcpConnection

-- ignore the rest
instance Eq TcpConnection where
  x == y = srcIp x == srcIp y && dstIp x == dstIp y
            && srcPort x == srcPort y && dstPort x == dstPort y
  -- /= = not ==

-- prevents hie from working correctly ?!
-- instance Default TcpConnection where
--   def = TcpConnection {
--         srcIp = fromIPv4 Net.IPv4.localhost
--         , dstIp = fromIPv4 Net.IPv4.localhost
--         , srcPort = 0
--         , dstPort = 0
--         , priority = Nothing
--         , localId = 0
--         , remoteId = 0
--         , inetFamily  = eAF_INET
--         , subflowInterface = Nothing
--       }

reverse :: TcpConnection -> TcpConnection
reverse con = TcpConnection {
  srcIp = dstIp con
  , dstIp = srcIp con
  , srcPort = dstPort con
  , dstPort = srcPort con
  , priority = Nothing
  , localId = remoteId con
  , remoteId = localId con
  , subflowInterface = Nothing
  , inetFamily = inetFamily con
}

-- data TcpSubflow = TcpSubflow {
-- }

instance FromJSON TcpConnection
instance ToJSON TcpConnection


type MptcpPacket = GenlPacket NoData


-- |Data to hold MPTCP level information
data MptcpConnection = MptcpConnection {
  connectionToken :: MptcpToken
  , subflows :: [TcpConnection]
  , localIds :: [Word8]  -- ^ Announced addresses
  , remoteIds :: [Word8]  -- ^ Announced addresses
} deriving (Show, Generic)


instance FromJSON MptcpConnection
instance ToJSON MptcpConnection


-- TODO add to localIds
mptcpConnAddSubflow :: MptcpConnection -> TcpConnection -> MptcpConnection
mptcpConnAddSubflow mptcpConn subflow =
  mptcpConn {
    subflows = subflow : subflows mptcpConn
    , localIds = localId subflow : localIds mptcpConn
    , remoteIds = remoteId subflow : remoteIds mptcpConn
  }

mptcpConnAddLocalId :: MptcpConnection
                       -> Word8 -- ^ Local id to add
                       -> MptcpConnection
mptcpConnAddLocalId con locId = undefined

-- mptcpConnAddAnnouncement

-- TODO remove subflow
mptcpConnRemoveSubflow :: MptcpConnection -> TcpConnection -> MptcpConnection
mptcpConnRemoveSubflow mptcpConn subflow = undefined

getPort :: ByteString -> Word16
getPort val =
  case (runGet getWord16host val) of
    Left _ -> 0
    Right port -> port



-- TODO merge default attributes
-- todo pass a list of (Int, Bytestring) and build the map with fromList ?
{-|
  Generates an Mptcp netlink request
-}
genMptcpRequest :: Word16 -- ^ the family id
                -> MptcpGenlEvent -- ^The MPTCP command
                -> Bool           -- ^Dump answer (returns EOPNOTSUPP if not possible)
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

    pkt = Packet myHeader (GenlData geheader NoData) (mptcpListToAttributes attrs)
    -- TODO run an assert on the list filter
    hasTokenAttr = Prelude.any (isAttribute (MptcpAttrToken 0)) attrs
  in
    assert hasTokenAttr pkt

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
  Just val -> case runGet getWord8 val of
    -- TODO generate an error here !
    Left _ -> 0
    Right locId -> locId
  -- runGet getWord8 val

-- inspectResult :: MyState -> Either String MptcpPacket -> IO()
-- inspectResult myState result =  case result of
--     Left ex -> putStrLn $ "An error in parsing happened" ++ show ex
--     -- Right myPack -> dispatchPacket myState myPack >> putStrLn "Valid packet"
--     Right myPack ->  putStrLn "inspect result Valid packet"


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

-- Wouldn't it be easier to work with ?
-- data MptcpEvent = NewConnection {
-- }


data MptcpAttribute =
    MptcpAttrToken MptcpToken |
    -- v4 or v6, AddressFamily is a netlink def
    SubflowFamily AddressFamily |
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
genV6SubflowAddress addr = undefined
-- (fromEnum MPTCP_ATTR_SADDR6, runPut $ putIPAddress addr)

mptcpListToAttributes :: [MptcpAttribute] -> Attributes
mptcpListToAttributes attrs = Map.fromList $map attrToPair attrs


-- mptcpAttributesToMap :: [MptcpAttribute] -> Attributes
-- mptcpAttributesToMap attrs =
--   Map.fromList $map mptcpAttributeToTuple attrs

-- TODO simplify
subflowFromAttributes :: Attributes -> TcpConnection
subflowFromAttributes attrs =
  -- makeAttribute Int ByteString
  let
    -- expects a ByteString
    (SubflowSourcePort sport) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_SPORT attrs
    (SubflowDestPort dport) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_DPORT attrs
    SubflowSourceAddress _srcIp = case makeAttributeFromMaybe MPTCP_ATTR_SADDR4 attrs of
      Just ip -> ip
      Nothing -> case makeAttributeFromMaybe MPTCP_ATTR_SADDR6 attrs of
        Just ip -> ip
        Nothing -> error "could not get the src IP"
    SubflowDestAddress _dstIp = case makeAttributeFromMaybe MPTCP_ATTR_DADDR4 attrs of
      Just ip -> ip
      Nothing -> case makeAttributeFromMaybe MPTCP_ATTR_DADDR6 attrs of
        Just ip -> ip
        Nothing -> error "could not get the dest IP"
    (LocalLocatorId lid) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_LOC_ID attrs
    (RemoteLocatorId rid) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_REM_ID attrs
    (SubflowInterface intfId) = fromJust $ makeAttributeFromMaybe MPTCP_ATTR_IF_IDX attrs
    sfFamily = getPort $ fromJust (Map.lookup (fromEnum MPTCP_ATTR_FAMILY) attrs)
    prio = Nothing   -- (SubflowPriority N)
  in
    TcpConnection _srcIp _dstIp sport dport prio lid rid eAF_INET (Just intfId)

-- makeSubflowFromAttributes ::

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

-- makeAttributeFromMap ::

-- makeAttributeFromList ::

-- TODO rename fromMap
makeAttributeFromMaybe :: MptcpAttr -> Attributes -> Maybe MptcpAttribute
makeAttributeFromMaybe attrType attrs =
  let res = Map.lookup (fromEnum attrType) attrs in
  case res of
    Nothing -> Nothing
    Just bytestring -> makeAttribute (fromEnum attrType) bytestring

-- | Builds an MptcpAttribute from
makeAttribute :: Int -- ^ MPTCP_ATTR_TOKEN value
                  -> ByteString
                  -> Maybe MptcpAttribute
makeAttribute i val =
  case toEnum i of
    MPTCP_ATTR_TOKEN -> Just (MptcpAttrToken $ readToken $ Just val)
    MPTCP_ATTR_FAMILY -> Just (SubflowFamily $ eAF_INET)
    MPTCP_ATTR_SADDR4 -> SubflowSourceAddress <$> fromIPv4 <$> e2M ( getIPv4FromByteString val)
    MPTCP_ATTR_DADDR4 -> SubflowDestAddress <$> fromIPv4 <$> e2M (getIPv4FromByteString val)
    MPTCP_ATTR_SADDR6 -> SubflowSourceAddress <$> fromIPv6 <$> e2M (getIPv6FromByteString val)
    MPTCP_ATTR_DADDR6 -> SubflowDestAddress <$> fromIPv6 <$> e2M (getIPv6FromByteString val)
    MPTCP_ATTR_SPORT -> SubflowSourcePort <$> port where port = e2M $ runGet getWord16host val
    MPTCP_ATTR_DPORT -> SubflowDestPort <$> port where port = e2M $ runGet getWord16host val
    MPTCP_ATTR_LOC_ID -> Just (LocalLocatorId $ readLocId $ Just val )
    MPTCP_ATTR_REM_ID -> Just (RemoteLocatorId $ readLocId $ Just val )
    -- | i == fromEnum MPTCP_ATTR_LOC_ID = Just (SubflowSourceAddress $ val )
    MPTCP_ATTR_IF_IDX ->
             case runGet getWord32be val of
                Right x -> Just $ SubflowInterface x
                _ -> Nothing
    MPTCP_ATTR_BACKUP -> trace "makeAttribute BACKUP" Nothing
    MPTCP_ATTR_ERROR -> trace "makeAttribute ERROR" Nothing
    MPTCP_ATTR_TIMEOUT -> undefined
    MPTCP_ATTR_CWND -> undefined
    MPTCP_ATTR_FLAGS -> trace "makeAttribute ATTR_FLAGS" Nothing
    MPTCP_ATTR_UNSPEC -> undefined


dumpAttribute :: Int -> ByteString -> String
dumpAttribute attrId value =
  -- TODO replace with makeAttribute followd by show ?
  -- traceShowId
  show $ makeAttribute attrId value

  -- attrStr = case  ( toEnum (fromIntegral attr)) of
  --     MPTCP_ATTR_UNSPEC -> "UNSPECIFIED"
  --     MPTCP_ATTR_TOKEN -> "token: " ++ show (readToken $ Just value)
  --     MPTCP_ATTR_FAMILY -> "family: " ++ show value
  --     MPTCP_ATTR_LOC_ID -> "Locator id: " ++ show (readLocId $ Just value)
  --     MPTCP_ATTR_REM_ID -> "Remote id: " ++ show value
  --     MPTCP_ATTR_SADDR4 -> "ipv4.src: " ++ show value
  --     MPTCP_ATTR_SADDR6 -> "ipv6.src: " ++ show value
  --     MPTCP_ATTR_DADDR4 -> "ipv4.dest: " ++ show value
  --     MPTCP_ATTR_DADDR6 -> "ipv6.dest: " ++ show value
  --     MPTCP_ATTR_SPORT -> "sport: " ++ show (getPort value)
  --     MPTCP_ATTR_DPORT -> "dport: " ++ show (getPort value)
  --     MPTCP_ATTR_BACKUP -> "backup" ++ show value
  --     MPTCP_ATTR_ERROR -> "Error: " ++ show value
  --     MPTCP_ATTR_FLAGS -> "Flags: " ++ show value
  --     MPTCP_ATTR_TIMEOUT -> "timeout: " ++ show value
  --     MPTCP_ATTR_IF_IDX -> "ifId: " ++ show value
  --     MPTCP_ATTR_CWND -> "cwnd: " ++ show value
  --     -- _ -> "unhandled case"
  -- in
    -- attrStr


checkIfSocketExistsPkt :: Word16 -> [MptcpAttribute]  -> MptcpPacket
checkIfSocketExistsPkt fid attributes =
    genMptcpRequest fid MPTCP_CMD_EXIST True attributes

-- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell?noredirect=1&lq=1
-- attrToPair ( SubflowSourcePort port) = (fromEnum MPTCP_ATTR_SPORT, runPut $ putWord8 loc)
isAttribute :: MptcpAttribute -- ^ to compare with
               -> MptcpAttribute -- ^to compare to
               -> Bool
isAttribute ref toCompare = fst (attrToPair toCompare) == fst (attrToPair ref)

hasLocAddr :: [MptcpAttribute] -> Bool
hasLocAddr attrs = Prelude.any (isAttribute (LocalLocatorId 0)) attrs

hasFamily :: [MptcpAttribute] -> Bool
hasFamily = Prelude.any (isAttribute (SubflowFamily eAF_INET))

-- need to prepare a request
-- type GenlPacket a = Packet (GenlData a)
-- REQUIRES: LOC_ID / TOKEN
resetConnectionPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
resetConnectionPkt (MptcpSocket sock fid) attrs = let
    cmd = MPTCP_CMD_REMOVE
  in
    assert (hasLocAddr attrs) $ genMptcpRequest fid MPTCP_CMD_REMOVE False attrs


subflowAttrs :: TcpConnection -> [MptcpAttribute]
subflowAttrs con = [
            LocalLocatorId $ localId con
            , RemoteLocatorId $ remoteId con
            -- TODO adapt
            , SubflowFamily $ eAF_INET  -- inetFamily con
            , SubflowDestAddress $ dstIp con
            , SubflowDestPort $ dstPort con
            , SubflowInterface localhostIntfIdx
            -- https://github.com/multipath-tcp/mptcp/issues/338
            , SubflowSourceAddress $ srcIp con
            ]


-- TODO pass a TcpConnection instead ?
capCwndAttr :: MptcpToken -> TcpConnection -> MptcpPacket
capCwndPkt token sf =
  let capSubflowAttrs = [
            MptcpAttrToken token
            , SubflowMaxCwnd 3
            -- This should not be necessary anymore ?
            , SubflowSourcePort $ srcPort sf
            ] ++ (subflowAttrs masterSf)

capCwndPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
capCwndPkt (MptcpSocket sock fid) attrs =
    assert (hasFamily attrs) pkt
    where
        pkt = genMptcpRequest fid MPTCP_CMD_SND_CLAMP_WINDOW False attrs
        -- attrs = [
        --     MptcpAttrToken token
        --     , SubflowFamily eAF_INET
        --     , LocalLocatorId 0
        --     -- TODO check emote locator ?
        --     , RemoteLocatorId 0
        --     , SubflowInterface localhostIntfIdx
        --     -- , SubflowInterface localhostIntfIdx
        --     ]
    -- putStrLn "while waiting for a real implementation"

-- sport/backup/intf are optional
-- family /loc id/remid/daddr/dport
-- TODO pass new subflow ?
newSubflowPkt :: MptcpSocket -> [MptcpAttribute] -> MptcpPacket
newSubflowPkt (MptcpSocket sock fid) attrs = let
    cmd = MPTCP_CMD_SUB_CREATE
    pkt = genMptcpRequest fid MPTCP_CMD_SUB_CREATE False attrs
  in
    assert (hasFamily attrs) pkt

-- announceLocalIdPkt ::
-- announceLocalIdPkt = 

