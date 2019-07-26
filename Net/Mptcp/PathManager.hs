{-
Trying to come up with a userspace abstraction for MPTCP path management

The default should deal

-- Need to deal with change in interface
-}
-- There should be 
-- OnInterfaceChange


-- we should have a list of Interfaces as well
--class PathManager a where
--  -- when a new master connection is established
--  --
--  onMasterEstablishement MptcpConnection [NetworkInterface]

module Net.Mptcp.PathManager (
    PathManager (..)
    , NetworkInterface(..)
    , AvailablePaths
    , ndiffports
) where


import Net.Mptcp
import Net.IP
import Data.Word (Word32)
import qualified Data.Map as Map

-- basically a retranscription of NLR.NAddrMsg
-- or SystemInterface ?
-- TODO add ifname ? flags ?
-- Rename to NetworkInterface / System ?
data NetworkInterface = NetworkInterface {
  ipAddress :: IP,   -- ^ Should be a list or a set
  interfaceName :: String,  -- ^ eth0 / ppp0
  interfaceId :: Word32  -- ^ refers to addrInterfaceIndex
} deriving Show



-- [NetworkInterface]
type AvailablePaths = Map.Map IP NetworkInterface


-- class AvailableIPsContainer a where


-- Reimplements
data PathManager = PathManager {
  name :: String
  , onMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
  -- , onAddrChange 

  -- should list advertised IPs
}

-- } deriving PathManager

-- fullmesh / ndiffports

ndiffports :: PathManager
ndiffports = PathManager {
  name = "ndiffports"
  , onMasterEstablishement = nportsOnMasterEstablishement
  -- , onAddrChange =
  -- , onLinkChange
}


-- per interface
-- (a -> b -> b) -> b -> t a -> b
genPkt :: MptcpSocket -> MptcpConnection -> NetworkInterface -> [MptcpPacket] -> [MptcpPacket]
genPkt mptcpSock con intf pkts =
    pkts ++ [newSubflowPkt mptcpSock newSubflowAttrs]
    where
        newSubflowAttrs = [
                MptcpAttrToken $ connectionToken con
            ]




{-
  Generate requests
-}
nportsOnMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
nportsOnMasterEstablishement mptcpSock con paths = do
  -- foldr :: (a -> b -> b) -> b -> Map k a -> b
  foldr (genPkt mptcpSock con) [] paths
    -- []

  where
    -- genPkt NetworkInterface
    -- let newSfPkt = newSubflowPkt mptcpSock newSubflowAttrs
    newSubflowAttrs = [
          MptcpAttrToken $ connectionToken con
        ]
  -- ++ (subflowAttrs $ masterSf { srcPort = 0 })
