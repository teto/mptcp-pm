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

-- we can later map ip to a proper type
type IPAddress = IPv4

-- showIPv4 :: IPv4 -> String
-- showIPv4 (IPv4 ip) = concat . intersperse "." $ showOctets
--   where
--     showOctets = map show $ word8s ip

instance Convertable IPv4 where
  -- putWord32host $ take 4 (src cust)
  -- encodeUtf8
  getPut x =  putByteString $ encodeUtf8 x
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
  addressBs <- getByteString (4*8)
  -- decodeUtf8 :: ByteString -> Maybe IPv4 
  return $ fromJust (decodeUtf8 addressBs)
