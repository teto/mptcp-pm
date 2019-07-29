{-|
Module      : TCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE DeriveGeneric #-}
module Net.Tcp (
    TcpConnection (..)
    , Net.Tcp.reverse
    , Net.Tcp.nameFromTcpConnection
) where


import Net.IP
import Data.Aeson
import Data.Word (Word8, Word16, Word32)
import GHC.Generics

data TcpConnection = TcpConnection {
  -- TODO use libraries to deal with that ? filter from the command line for instance ?
  srcIp :: IP -- ^Source ip
  , dstIp :: IP -- ^Destination ip
  , srcPort :: Word16  -- ^ Source port
  , dstPort :: Word16  -- ^Destination port
  , priority :: Maybe Word8 -- ^subflow priority
  , localId :: Word8  -- ^ Convert to AddressFamily
  , remoteId :: Word8
  -- TODO remove could be deduced from srcIp / dstIp ?
  , subflowInterface :: Maybe Word32 -- ^Interface of Maybe ?
  -- add TcpMetrics member

} deriving (Show, Generic)


nameFromTcpConnection :: TcpConnection -> String
nameFromTcpConnection con =
  show (srcIp con) ++ ":" ++ show (srcPort con) ++ " -> " ++ show (dstIp con) ++ ":" ++ show (dstPort con)


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
}


instance FromJSON TcpConnection
instance ToJSON TcpConnection

-- ignore the rest
instance Eq TcpConnection where
  x == y = srcIp x == srcIp y && dstIp x == dstIp y
            && srcPort x == srcPort y && dstPort x == dstPort y
  -- /= = not ==

