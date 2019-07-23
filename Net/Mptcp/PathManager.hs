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
) where


import Net.Mptcp

-- Reimplements
data PathManager = PathManager {
  name :: String
  , onMasterEstablishement :: MptcpConnection [NetworkInterface] -> [MptcpPacket]

}

-- } deriving PathManager

-- fullmesh / ndiffports 

ndiffports = PathManager {
  name = "ndiffports"
  , onMasterEstablishement = nportsOnMasterEstablishement
}

nportsOnMasterEstablishement :: MptcpConnection [NetworkInterface] -> [MptcpPacket]
nportsOnMasterEstablishement = 
