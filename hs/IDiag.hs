{-|
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

import Data.Serialize.Get
import Data.Serialize.Put

-- for replicateM
import Control.Monad

-- for Convertable
import System.Linux.Netlink
-- For TcpState, FFI generated
import Generated (IDiagExt, TcpState)

import Data.Bits (shiftL, )
import Data.Bits ((.|.))

data InetDiagSockId  = InetDiagSockId  {
  sport :: Word16
  , dport :: Word16
  -- IP address, 4*
  , src :: [Word32]
  , dst :: [Word32]

  , intf :: Word32
  -- * 2
  , cookie :: [Word32]

} deriving (Eq, Show)

-- converts TcpState into Bits
shiftCustom :: TcpState -> Word32
shiftCustom state = shiftL 1 (fromEnum state - 1)

tcpStatesToWord :: [TcpState] -> Word32
tcpStatesToWord [] = 0
tcpStatesToWord (x:xs) = (shiftCustom x) .|. (tcpStatesToWord xs)

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

instance Convertable [TcpState] where
  getPut = putWord32be . tcpStatesToWord
  getGet _ = []

instance Convertable InetDiagMsg where
  getPut = putInetDiagMsg
  -- MessageType
  getGet _ = getInetDiagMsg

-- TODO rename to a TCP one ? SockDiagRequest
data SockDiagRequest = SockDiagRequest {
  sdiag_family :: Word8 -- ^AF_INET6 or AF_INET (TODO rename)
-- It should be set to the appropriate IPPROTO_* constant for AF_INET and AF_INET6, and to 0 otherwise.
  , sdiag_protocol :: Word8 -- ^IPPROTO_XXX always TCP ?
  -- IPv4/v6 specific structure
  , idiag_ext :: IDiagExt -- ^query extended info (word8 size)
  , req_pad :: Word8        -- ^ padding for backwards compatibility with v1

  -- c la ou ca foire
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
    return $ SockDiagRequest addressFamily protocol (toEnum (fromIntegral extended) :: IDiagExt) _pad [] _sockid

-- |'Put' function for 'GenlHeader'
putSockDiagRequestHeader :: SockDiagRequest -> Put
putSockDiagRequestHeader request = do
  -- let states = tcpStatesToWord $ idiag_states request
  putWord8 $ sdiag_family request
  putWord8 $ sdiag_protocol request
  -- extended
  putWord8 $ fromIntegral $ fromEnum $ idiag_ext request
  putWord8 $ req_pad request
  -- TODO check endianness
  putWord32be $ idiag_states request
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
    _src <- replicateM 4 getWord32host
    _dst <- replicateM 4 getWord32host
    _intf <- getWord32host
    _cookie <- replicateM 2 getWord32host
    return $ InetDiagSockId _sport _dport _src _dst _intf _cookie


putInetDiagSockid :: InetDiagSockId -> Put
putInetDiagSockid cust = do
  -- we might need to clean up this a bit
  putWord16be $ sport cust
  putWord16be $ sport cust
  -- TODO fix
  mapM_ putWord32host $ take 4 (src cust)
  mapM_ putWord32host $ take 4 (dst cust)
  -- replicateM 4 (putWord32be $ dst cust) -- dest
  putWord32host $ intf cust

  -- cookie ?
  mapM_ putWord32host $ take 2 (cookie cust)
  -- replicateM $ putWord32host (cookie cust) 4

