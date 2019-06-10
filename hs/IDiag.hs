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
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude hiding (length, concat)
import Prelude hiding (length, concat)

import Data.Serialize
import Data.Serialize.Get ()
import Data.Serialize.Put ()

-- for replicateM
-- import Control.Monad

import System.Linux.Netlink
import System.Linux.Netlink.Constants
-- For TcpState, FFI generated
import Generated
-- (IDiagExt, TcpState, msgTypeSockDiag)

import qualified Data.Bits as B
import Data.Bits ((.|.))
import qualified Data.Map as Map
import Data.ByteString ()
-- import Data.ByteString.Char8 as C8 (pack)
-- import Data.IP
import Net.IPAddress
import Net.IP ()
import Net.IPv4
import Data.ByteString (ByteString)

-- iproute uses this seq number #define MAGIC_SEQ 123456
magicSeq :: Word32
magicSeq = 123456

-- toIpv4 :: ByteString -> String
-- toIpv4 val = Data.List.intercalate "." ( map show (unpack val))


data InetDiagSockId  = InetDiagSockId  {
  idiag_sport :: Word16
  , idiag_dport :: Word16

  -- Just be careful that this is a fixed size regardless of family
  -- __be32  idiag_src[4];
  -- __be32  idiag_dst[4];
  -- we don't know yet the address family
  , idiag_src :: ByteString
  , idiag_dst :: ByteString

  , idiag_intf :: Word32
  -- * 2
  , idiag_cookie :: Word64

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

-- TODO use bitset package ? but broken on nixos
wordToEnums :: Enum2Bits a =>  Word32 -> [a]
wordToEnums  _ = []

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
    return $ SockDiagRequest addressFamily protocol 
      (wordToEnums extended :: [IDiagExt]) (wordToEnums states :: [TcpState])  _sockid

-- |'Put' function for 'GenlHeader'
putSockDiagRequestHeader :: SockDiagRequest -> Put
putSockDiagRequestHeader request = do
  -- let states = enumsToWord $ idiag_states request
  putWord8 $ sdiag_family request
  putWord8 $ sdiag_protocol request
  -- extended todo use Enum2Bits
  --putWord32host $ enumsToWord states
  putWord8 ( fromIntegral (enumsToWord $ idiag_ext request) :: Word8)
  putWord8 0  -- padding ?
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
    sport <- getWord16host
    dport <- getWord16host
    -- iterate/ grow
    _src <- getByteString (4*4)
    _dst <- getByteString (4*4)
    _intf <- getWord32host
    cookie <- getWord64host
    return $ InetDiagSockId sport dport _src _dst _intf cookie

putInetDiagSockid :: InetDiagSockId -> Put
putInetDiagSockid cust = do
  -- we might need to clean up this a bit
  putWord16be $ idiag_sport cust
  putWord16be $ idiag_dport cust
  putByteString (idiag_src cust)
  putByteString (idiag_dst cust)
  -- putIPAddress (src cust)
  -- putIPAddress (dst cust)
  putWord32host $ idiag_intf cust
  putWord64host $ idiag_cookie cust

-- include/uapi/linux/inet_diag.h
-- struct tcpvegas_info {
-- 	__u32	tcpv_enabled;
-- 	__u32	tcpv_rttcnt;
-- 	__u32	tcpv_rtt;
-- 	__u32	tcpv_minrtt;
-- };
data DiagTcpInfo = TcpVegasInfo {
  -- TODO hide ?
  tcpInfoVegasEnabled :: Word32
  , tcpInfoRttCount :: Word32
  , tcpInfoRtt :: Word32
  , tcpInfoMinrtt :: Word32
}

instance Convertable DiagTcpInfo where
  getPut  = putDiagTcpInfo
  getGet _  = getDiagTcpInfo


putDiagTcpInfo :: DiagTcpInfo -> Put
putDiagTcpInfo info = error "should not be needed"


getDiagTcpInfo :: Get DiagTcpInfo
getDiagTcpInfo =
  TcpVegasInfo <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

-- TODO generate via FFI ?
eIPPROTO_TCP :: Word8
eIPPROTO_TCP = 6

-- __u32	idiag_rmem;
-- __u32	idiag_wmem;
-- __u32	idiag_fmem;
-- __u32	idiag_tmem;
data Meminfo = Meminfo {
  idiag_rmem :: Word32
, idiag_wmem :: Word32
, idiag_fmem :: Word32
, idiag_tmem :: Word32
}
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
-- TODO if we have a cookie ignore the rest ?!
-- requestedInfo = InetDiagNone
genQueryPacket :: (Maybe Word64) -> [TcpState] -> [IDiagExt] -> Packet SockDiagRequest
genQueryPacket cookie tcpStatesFilter requestedInfo = let
  -- Mesge type / flags /seqNum /pid
  flags = (fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT)

  -- might be a trick with seqnum
  hdr = Header msgTypeSockDiag flags magicSeq 0
  -- global for now
  iperfSrcPort = 0
  iperfDstPort = 0
  -- iperfSrcPort = iperfHardcodedSrcPort
  -- iperfDstPort = 5201
  _cookie = 0 :: Word64

  -- TODO 
  -- ipSrc = Data.Serialize.encode $ fromIPv4 localhost
  ipSrc = runPut $ putIPv4Address localhost
  ipDst = runPut $ putIPv4Address localhost
  -- ipDst = fromIPv4 localhost
  -- 1 => "lo". Check with ip link ?
  ifIndex = fromIntegral localhostIntfIdx :: Word32
  diag_req = InetDiagSockId iperfSrcPort iperfDstPort ipSrc ipDst ifIndex _cookie

  custom = SockDiagRequest eAF_INET eIPPROTO_TCP requestedInfo tcpStatesFilter diag_req
  in
    Packet hdr custom Map.empty

queryPacketFromCookie :: Word64 -> Packet SockDiagRequest
queryPacketFromCookie cookie =  genQueryPacket (Just cookie) [] []
