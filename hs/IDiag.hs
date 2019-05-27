{-|
    ipSrc <- IPAddress . pack <$> replicateM (4*8) getWord8
    -- ipSrc = IPAddress . pack <$> replicateM (4*8) getWord8
    ipDst <- IPAddress . pack <$> replicateM (4*8) getWord8 
Module      : IDiag
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE FlexibleInstances #-}
module IDiag
where

-- import Generated
import Data.Word (Word8, Word16, Word32)

import Prelude hiding (length, concat)
import Prelude hiding (length, concat)

import Data.Serialize.Get
import Data.Serialize.Put


-- for replicateM
import Control.Monad

import System.Linux.Netlink
import System.Linux.Netlink.Constants
-- For TcpState, FFI generated
import Generated
-- (IDiagExt, TcpState, msgTypeSockDiag)

import qualified Data.Bits as B
import Data.Bits ((.|.))
import qualified Data.Map as Map
import Data.ByteString (ByteString, pack)
-- import Data.ByteString.Char8 as C8 (pack)
-- import Data.IP
-- import Net.IP
import Net.IPAddress
import Net.IPv4

-- iproute uses this seq number #define MAGIC_SEQ 123456
magicSeq :: Word32
magicSeq = 123456

-- toIpv4 :: ByteString -> String
-- toIpv4 val = Data.List.intercalate "." ( map show (unpack val))


data InetDiagSockId  = InetDiagSockId  {
  sport :: Word16
  , dport :: Word16

  -- Just be careful that this is a fixed size regardless of family
  -- __be32  idiag_src[4];
  -- __be32  idiag_dst[4];
  , src :: IPAddress
  , dst :: IPAddress

  , intf :: Word32
  -- * 2
  , cookie :: [Word32]

} deriving (Eq, Show)


-- TODO we need a way to rebuild from the integer to the enum
class Enum2Bits a where
  -- toBits :: [a] -> Word32
  shiftL :: a -> Word32

instance Enum2Bits TcpState where
  -- toBits = enumsToWord
  shiftL state = B.shiftL 1 (fromEnum state)

instance Enum2Bits IDiagExt where
  -- toBits = enumsToWord
  shiftL state = B.shiftL 1 ((fromEnum state) - 1)

-- instance Enum2Bits INetDiag where
--   toBits = enumsToWord


enumsToWord :: Enum2Bits a => [a] -> Word32
enumsToWord [] = 0
enumsToWord (x:xs) = (shiftL x) .|. (enumsToWord xs)

-- This generates a response of inet_diag_msg
-- rename to answer ?
data InetDiagMsg = InetDiagMsg {
  idiag_family :: Word8
  , idiag_state :: Word8
  , idiag_timer :: Word8
  , idiag_retrans :: Word8
  , idiag_sockid :: InetDiagSockId
  , idiag_expires :: Word32
  , idiag_rqueue :: Word32
  , idiag_wqueue :: Word32
  , idiag_uid :: Word32
  , idiag_inode :: Word32
} deriving (Eq, Show)

-- see https://stackoverflow.com/questions/8633470/illegal-instance-declaration-when-declaring-instance-of-isstring
{-# LANGUAGE FlexibleInstances #-}

-- TODO this generates the  error "Orphan instance: instance Convertable [TcpState]"
-- instance Convertable [TcpState] where
--   getPut = putStates
--   getGet _ = return []

putStates :: [TcpState] -> Put
putStates states = putWord32host $ enumsToWord states


instance Convertable InetDiagMsg where
  getPut = putInetDiagMsg
  getGet _ = getInetDiagMsg

-- TODO rename to a TCP one ? SockDiagRequest
data SockDiagRequest = SockDiagRequest {
  sdiag_family :: Word8 -- ^AF_INET6 or AF_INET (TODO rename)
-- It should be set to the appropriate IPPROTO_* constant for AF_INET and AF_INET6, and to 0 otherwise.
  , sdiag_protocol :: Word8 -- ^IPPROTO_XXX always TCP ?
  -- IPv4/v6 specific structure
  , idiag_ext :: [IDiagExt] -- ^query extended info (word8 size)
  -- , req_pad :: Word8        -- ^ padding for backwards compatibility with v1

  -- in principle, any kind of state, but for now we only deal with TcpStates
  , idiag_states :: [TcpState] -- ^States to dump (based on TcpDump) Word32
  , diag_sockid :: InetDiagSockId -- ^inet_diag_sockid 
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
    extended <- getWord32host
    _pad <- getWord8
    -- TODO discarded later
    states <- getWord32host
    _sockid <- getInetDiagSockid
    -- TODO reestablish states
    return $ SockDiagRequest addressFamily protocol (toEnum (fromIntegral extended) :: IDiagExt) [] _sockid

-- |'Put' function for 'GenlHeader'
putSockDiagRequestHeader :: SockDiagRequest -> Put
putSockDiagRequestHeader request = do
  -- let states = enumsToWord $ idiag_states request
  putWord8 $ sdiag_family request
  putWord8 $ sdiag_protocol request
  -- extended todo use Enum2Bits
  putWord8 $ fromIntegral $ fromEnum $ idiag_ext request
  putWord8 0
  -- TODO check endianness
  putStates $ idiag_states request
  putInetDiagSockid $ diag_sockid request

getInetDiagMsg :: Get InetDiagMsg
getInetDiagMsg  = do
    family <- getWord8
    state <- getWord8
    timer <- getWord8
    retrans <- getWord8

    _sockid <- getInetDiagSockid
    expires <- getWord32host
    rqueue <- getWord32host
    wqueue <- getWord32host
    uid <- getWord32host
    inode <- getWord32host
    return$  InetDiagMsg family state timer retrans _sockid expires rqueue wqueue uid inode

putInetDiagMsg :: InetDiagMsg -> Put
putInetDiagMsg msg = do
  putWord8 $ idiag_family msg
  putWord8 $ idiag_state msg
  putWord8 $ idiag_timer msg
  putWord8 $ idiag_retrans msg

  putInetDiagSockid $ idiag_sockid msg

  -- Network order
  putWord32le $ idiag_expires msg
  putWord32le $ idiag_rqueue msg
  putWord32le $ idiag_wqueue msg
  putWord32le $ idiag_uid msg
  putWord32le $ idiag_inode msg

-- TODO add support for OWDs
getInetDiagSockid :: Get InetDiagSockId
getInetDiagSockid  = do
-- getWord32host
    _sport <- getWord16host
    _dport <- getWord16host
    -- iterate/ grow
    _src <- getIPAddress
    _dst <- getIPAddress
    _intf <- getWord32host
    _cookie <- replicateM 2 getWord32host
    return $ InetDiagSockId _sport _dport _src _dst _intf _cookie

putInetDiagSockid :: InetDiagSockId -> Put
putInetDiagSockid cust = do
  -- we might need to clean up this a bit
  putWord16be $ sport cust
  putWord16be $ sport cust
  -- TODO fix
  --
  getPut (src cust)
  getPut (dst cust)

  -- replicateM 4 (putWord32be $ dst cust) -- dest
  putWord32host $ intf cust

  -- cookie ?
  mapM_ putWord32host $ take 2 (cookie cust)
  -- replicateM $ putWord32host (cookie cust) 4



-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6

-- IPAddressFromString :: String -> IPv4
-- IPAddressFromString = decodeString

-- fromOctets :: [Word8] -> Word32
-- fromOctets = foldl' accum 0
--   where
--     accum a o = (a `shiftL` 8) .|. fromIntegral o

-- Sends a SockDiagRequest
-- expects INetDiag
-- TODO should take an Mptcp connection into account
-- We should use cookies later on
-- MaybeCookie ?
-- TcpConnection -- ^Connection we are requesting
-- #define SS_ALL ((1 << SS_MAX) - 1)
-- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
-- stateFilter = [TcpListen, TcpEstablished, TcpSynSent ]

-- InetDiagInfo
-- TODO we need to request more !
-- requestedInfo = InetDiagNone
genQueryPacket :: [TcpState] -> [IDiagExt] -> Packet SockDiagRequest
genQueryPacket tcpStatesFilter requestedInfo = let
  -- Mesge type / flags /seqNum /pid
  flags = (fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT)

  -- might be a trick with seqnum
  hdr = Header msgTypeSockDiag flags magicSeq 0
  -- IPPROTO_TCP = 6,
  -- -1 => ignore cookie content
  -- _cookie = [ maxBound :: Word32, maxBound :: Word32]
  -- plutot un tuple ou un word64 si ca existe ?
  _cookie = [ 0 :: Word32, 0 :: Word32]

  -- global for now
  iperfSrcPort = 0
  iperfDstPort = 0
  -- iperfSrcPort = iperfHardcodedSrcPort
  -- iperfDstPort = 5201
  -- 4 Word32
  -- ipSrc = [ fromOctetsLE [ 127, 0, 0, 1], 0, 0, 0]
-- C8.pack [ 0 , 0, 0, 0]
-- look at fmap
  ipSrc = localhost
  ipDst = localhost
  -- ipDst = IPAddress <$> pack <$> replicateM (4*8) getWord8
  -- 1 => "lo". Check with ip link ?
  -- ifIndex = IDiag.fromOctets [ 0, 0, 0, interfaceIdx]
  ifIndex = fromIntegral interfaceIdx :: Word32
  -- diag_req = InetDiagSockId iperfSrcPort iperfDstPort ipSrc ipDst ifIndex _cookie
  diag_req = InetDiagSockId iperfSrcPort iperfDstPort ipSrc ipDst ifIndex _cookie

  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo tcpStatesFilter diag_req
  in
    Packet hdr custom Map.empty

-- queryPacketFromCookie = 
