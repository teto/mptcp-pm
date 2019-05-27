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
import Net.IPv4
import System.Linux.Netlink
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Maybe (fromJust)
import Data.Word (Word8)

-- for replicateM
import Control.Monad

-- we can later map ip to a proper type
type IPAddress = IPv4

-- showIPv4 :: IPv4 -> String
-- showIPv4 (IPv4 ip) = concat . intersperse "." $ showOctets
--   where
--     showOctets = map show $ word8s ip

-- check ip link / localhost seems to be 1
-- global interface index
interfaceIdx :: Word8
interfaceIdx = 1

instance Convertable IPv4 where
  -- putWord32host $ take 4 (src cust)
  -- encodeUtf8
  getPut =  putIPAddress
  -- MessageType / getSockDiagRequestHeader
  getGet _ = getIPAddress

-- TODO clean it up
-- getNested Get Int Get a / getListOf Get a
-- _dst <- replicateM 4 getWord32host
getIPAddress :: Get IPAddress
getIPAddress = do
  --  Fails if fewer than n bytes are left in the input
  -- 4 Word32
  -- IPAddress . pack <$> replicateM (4*8) getWord8

  -- in number of bytes for 4 Word32
  addressBs <- getByteString (4*4)
  -- decodeUtf8 :: ByteString -> Maybe IPv4
  case decodeUtf8 addressBs of
    Nothing -> error "could not decode ip"
    Just ip -> return ip
  -- return $ fromJust (decodeUtf8 addressBs)

-- big endian for IDiag
-- replicateM 4 (putWord32be $ dst cust) -- dest
-- assuming it's an ipv4
putIPAddress :: IPAddress -> Put
putIPAddress addr =
    -- (Word8, Word8, Word8, Word8)
    let
      -- hack
      (ip1, ip2, ip3, ip4) = toOctets addr
    in do
      -- mapM_ putWord8 t
      putWord8 ip1
      putWord8 ip2
      putWord8 ip3
      putWord8 ip4
      replicateM_ 3 (putWord32host 0)
  --
  -- putByteString (encodeUtf8 addr)
  -- putWord32host 0
  -- putWord32host 0
  -- putWord32host 0
  -- -- replicateM 3 (putWord32host 0)


