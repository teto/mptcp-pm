module Net.Mptcp.PathManager.Default (
    -- TODO don't export / move to its own file
    ndiffports
    , meshPathManager
) where

import Net.Tcp
import Net.Mptcp
import Net.Mptcp.PathManager
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace

ndiffports :: PathManager
ndiffports = PathManager {
  name = "ndiffports"
  , onMasterEstablishement = nportsOnMasterEstablishement
}

meshPathManager :: PathManager
meshPathManager = PathManager {
  name = "mesh"
  , onMasterEstablishement = meshOnMasterEstablishement
}



-- per interface
--  TODO check if there is already an interface with this connection
meshGenPkt :: MptcpSocket -> MptcpConnection -> NetworkInterface -> [MptcpPacket] -> [MptcpPacket]
meshGenPkt mptcpSock mptcpCon intf pkts =

    if traceShow (intf) (interfaceId intf == (fromJust $ subflowInterface masterSf)) then
        pkts
    else
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

        masterSf = Set.elemAt 0 (subflows mptcpCon)


{-
  Generate requests
it iterates over local interfaces and try to connect
-}
meshOnMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
meshOnMasterEstablishement mptcpSock con paths = do
  foldr (meshGenPkt mptcpSock con) [] paths


{-
  Generate requests
TODO it iterates over local interfaces but not 
-}
nportsOnMasterEstablishement :: MptcpSocket -> MptcpConnection -> AvailablePaths -> [MptcpPacket]
nportsOnMasterEstablishement mptcpSock con paths = do
  foldr (meshGenPkt mptcpSock con) [] paths
  -- TODO create #X subflows
  -- iterate 

