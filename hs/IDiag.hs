{-|
Module      : 
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux


-}
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


-- This generates a response of inet_diag_msg
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


instance Convertable InetDiagMsg where
  getPut = putInetDiagMsg
  -- MessageType
  getGet _ = getInetDiagMsg

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

-- TODO
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

