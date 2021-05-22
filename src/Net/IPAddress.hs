-- |
-- Description
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Net.IPAddress
where
import Net.IP
import Net.IPv4
import Net.IPv6
import Data.Serialize.Get
import Data.Serialize.Put
import Data.ByteString
import System.Linux.Netlink.Constants as NLC

import Control.Monad


getIPv4FromByteString :: ByteString -> Either String IPv4
getIPv4FromByteString val =
  runGet (Net.IPv4.fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8) val


-- |
getIPFromByteString :: NLC.AddressFamily -> ByteString -> Either String IP
getIPFromByteString addrFamily ipBstr
  | addrFamily == eAF_INET = fromIPv4 <$> getIPv4FromByteString ipBstr
  | addrFamily == eAF_INET6 = fromIPv6 <$> getIPv6FromByteString ipBstr
  | otherwise = error $ "unsupported addrFamily " ++ show addrFamily


getIPv6FromByteString :: ByteString -> Either String IPv6
getIPv6FromByteString bs =
  let
    val = Net.IPv6.fromWord32s <$> getWord32be <*> getWord32be <*> getWord32be <*> getWord32be
  in
    runGet val bs


putIPAddress :: IP -> Put
putIPAddress addr =
  case_ putIPv4Address putIPv6Address addr

-- the doc should show the MSB
putIPv6Address :: IPv6 -> Put
putIPv6Address addr =
  let
    (w1, w2, w3, w4) = toWord32s addr
  in do
    putWord32be w1
    putWord32be w2
    putWord32be w3
    putWord32be w4

-- |IDIag version since it will add some padding to reach 128 bits
putIPv4Address :: IPv4 -> Put
putIPv4Address addr =
    let
      w32 = getIPv4 addr
    in do
      putWord32be w32
      replicateM_ 3 (putWord32be 0)


getAddressFamily :: IP -> AddressFamily
getAddressFamily = case_ (const eAF_INET) (const eAF_INET6)

-- isIPv6 :: IP -> Bool
-- isIPv6 = case_ (const False) (const True)
