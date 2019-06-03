-- |
-- Module      :  Ip
-- Copyright   :  teto 2019
-- License     :  GPL3
--
-- Description
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Net.IPAddress
where
import Net.IP
import Net.IPv4
import Net.IPv6
import System.Linux.Netlink
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Maybe (fromJust)
import Data.Word (Word8, Word32)
import Data.ByteString (ByteString)

-- for replicateM
import Control.Monad


-- check ip link / localhost seems to be 1
-- global interface index
localhostIntfIdx :: Word32
localhostIntfIdx = 1


-- then I could do encode myIP
-- instance Convertable IP where
--   getPut =  putIPAddress
--   getGet _ = getIPAddress

-- TODO I could use Serialize for IP instead
-- instance Serialize IP where

getIPv4FromByteString :: ByteString -> Either String IPv4
getIPv4FromByteString val =
  runGet (Net.IPv4.fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8) val


getIPv6FromByteString :: ByteString -> Either String IPv6
getIPv6FromByteString bs =
  -- returns a list to me
  -- <$> replicateM 16 (getWord8) in
  -- TODO check ?
  let
    val = Net.IPv6.fromWord32s <$> getWord32be <*> getWord32be <*> getWord32be <*> getWord32be
  in
    runGet val bs

-- TODO pass the type ?
-- getIPAddressFromByteString :: ByteString -> Maybe IP
-- getIPAddressFromByteString bstr =
--   let
--     -- val = (fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8)
--     -- probably could use Word32 instead ?
--   in
--   case runGet val bstr  of
--     Left err -> error "maybe it was an ipv6"
--     Right ip -> Just $ fromIPv4 ip
    --
    -- fromIPv6 c


-- big endian for IDiag
-- replicateM 4 (putWord32be $ dst cust) -- dest
-- assuming it's an ipv4
-- valid for the ip v 4 only
-- one constructor is getIPv4 :: Word32
putIPAddress :: IP -> Put
putIPAddress addr =
  case_ putIPv4Address (\x -> undefined) addr


-- |IDIag version since it will add some padding
putIPv4Address :: IPv4 -> Put
putIPv4Address addr =
    let
      w32 = getIPv4 addr
      -- (ip1, ip2, ip3, ip4) = toOctets $ addr
    in do
      putWord32be w32
      -- mapM_ putWord8 t
      -- putWord8 ip1
      -- putWord8 ip2
      -- putWord8 ip3
      -- putWord8 ip4
      replicateM_ 3 (putWord32be 0)


