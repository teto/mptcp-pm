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
import System.Linux.Netlink
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.ByteString (ByteString)

-- for replicateM
import Control.Monad


-- check ip link / localhost seems to be 1
-- global interface index
interfaceIdx :: Word8
interfaceIdx = 1


-- TODO ipv4 / ipv6 ?
-- use case_ :: (IPv4 -> a) -> (IPv6 -> a) -> IP -> a  to do 
instance Convertable IP where
  -- putWord32host $ take 4 (src cust)
  -- encodeUtf8
  getPut =  putIPAddress
  -- MessageType / getSockDiagRequestHeader
  getGet _ = getIPAddress


-- TODO clean it up
-- getNested Get Int Get a / getListOf Get a
-- _dst <- replicateM 4 getWord32host
getIPAddress :: Get IP
getIPAddress = do
  -- I believe this is wrong
  -- Use from octets instead ?
  -- ip <- fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  -- or we could use skip
  -- getBytes and use getIPAddressFromByteString on it
  bstr <- getByteString (4*4)
  -- _ <- getWord32host
  -- _ <- getWord32host
  -- _ <- getWord32host

  return $ fromJust  $ getIPAddressFromByteString bstr

  -- addressBs <- getByteString (4*4)
  -- -- decodeUtf8 :: ByteString -> Maybe IPv4
  -- case decodeUtf8 addressBs of
  --   Nothing -> error "could not decode ip"
  --   Just ip -> return ip
  -- return $ fromJust (decodeUtf8 addressBs)

-- TODO pass the type ?
getIPAddressFromByteString :: ByteString -> Maybe IP
getIPAddressFromByteString bstr =
  -- use fromOctets Word8 Word8 Word8 Word8
  let
    -- Get IP
    -- fromOctets -> IPv4
    val = (fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8)
  in
  case runGet val bstr  of
    Left err -> error "maybe it was an ipv6"
    Right ip -> Just $ fromIPv4 ip

-- big endian for IDiag
-- replicateM 4 (putWord32be $ dst cust) -- dest
-- assuming it's an ipv4
-- valid for the ip v 4 only
-- one constructor is getIPv4 :: Word32
putIPAddress :: IP -> Put
putIPAddress addr =
  case_ putIPv4Address (\x -> undefined) addr

putIPv4Address :: IPv4 -> Put
putIPv4Address addr =
    let
      (ip1, ip2, ip3, ip4) = toOctets $ addr
    in do
      -- mapM_ putWord8 t
      putWord8 ip1
      putWord8 ip2
      putWord8 ip3
      putWord8 ip4
      replicateM_ 3 (putWord32host 0)


