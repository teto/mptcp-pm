module Net.Mptcp.PathManager.Default (
    -- TODO don't export / move to its own file
    ndiffports
) where

import Net.Tcp
import Net.Mptcp
import Net.Mptcp.PathManager

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
genPkt mptcpSock mptcpCon intf pkts =
    pkts ++ [newSubflowPkt mptcpSock mptcpCon generatedCon]
    where
        generatedCon = TcpConnection {
          srcPort = 0  -- let the kernel handle it
          , dstPort = dstPort masterSf
          , srcIp = ipAddress intf
          , dstIp =  dstIp masterSf  -- same as master
          , priority = Nothing
          -- TODO fix this
          , localId = fromIntegral $ interfaceId intf    -- how to get it ? or do I generate it ?
          , remoteId = remoteId masterSf
          , subflowInterface = Just $ interfaceId intf
        }

        masterSf = head $ subflows mptcpCon

        -- newSubflowAttrs = [
        --         MptcpAttrToken $ connectionToken con
        --     ]



{-
  Generate requests
-}
nportsOnMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
nportsOnMasterEstablishement mptcpSock con paths = do
  -- foldr :: (a -> b -> b) -> b -> Map k a -> b
  foldr (genPkt mptcpSock con) [] paths

