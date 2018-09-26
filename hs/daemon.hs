-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h
-}
module Main where

import Prelude hiding (length, concat)
import Options.Applicative
import Data.Semigroup ((<>))

-- import Data.ByteString (ByteString, length, unpack, pack, concat)

import System.Environment (getArgs)

import System.Linux.Netlink hiding (makeSocket)
import System.Linux.Netlink.GeNetlink (makeSocket, GenlPacket )
-- import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.Utils

import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word16, Word8)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
-- data MptcpSocket = NLS NetlinkSocket Word16


data Command = MPTCP_CMD_ANNOUNCE | MPTCP_CMD_REMOVE | MPTCP_CMD_SUB_CREATE 
  deriving  (Enum)

-- en fait y a 2 multicast groups
-- genl_multicast_group 

mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName = "mptcp_events"
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = "mptcp_commands"
mptcpGenlName :: String
mptcpGenlName="mptcp"

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

-- MPTCP_GENL_CMD_GRP_NAME  = "mptcp_commands"
-- MPTCP_GENL_VERSION       = 1
-- MPTCP_FAMILY_NAME   = MPTCP_GENL_NAME = "mptcp"

-- NetlinkSocket
-- makeSocketGeneric
-- let active = take 1 args == ["--active"] 

-- NetlinkSocket
-- mptcpNetlink :: IO ()
-- mptcpNetlink = 
--   let 
--     -- family_id = getFamilyId sock mptcpGenlEvGrpName
--     -- getFamilyId vs getMulticast
--     -- getFamilyIdS is the safe version returning a Maybe
--     -- joinMulticastGroup
--     genlFamilyId = makeSocket >>= \sock -> (getFamilyId sock mptcp_genl_name)
--     -- genlFamilyId = makeSocket >>= \sock -> (getMulticast sock mptcpGenlEvGrpName)
--   in
--     -- expects a word32
--     System.Linux.Netlink.GeNetlink.join sock familyId

-- inspired by makeNL80211Socket Create a 'NL80211Socket' this opens a genetlink 
-- socket and gets the family id
-- makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket :: IO (NetlinkSocket, Word16)
makeMptcpSocket = do
  sock <- makeSocket
  fid <- getFamilyId sock mptcpGenlName
  -- return $NLS sock fid
  return ( sock, fid)


-- 	err = genl_ctrl_grp_by_name(family, grp_name);
-- 	genl_family_put(family);
--
-- errout:
-- 	return err;


inspectPacket :: GenlPacket NoData -> IO ()
inspectPacket packet = do

  putStrLn $show packet
  -- return IO
  -- case pack of
  --   DoneMsg -> putStrLn "Done Msg"
  --   System.Linux.Netlink.ErrorMsg -> error "error msg"
  --   Packet -> putStrLn $ packetHeader pack

-- CtrlAttrMcastGroup
-- copied from utils/GenlInfo.hs
doDumpLoop :: NetlinkSocket -> IO ()
doDumpLoop sock = do
  -- type GenlPacket a = Packet (GenlData a) 
  -- with data GenlData A wrapper around GenlHeader
  -- genlDataHeader
  pack <- (recvOne sock :: IO [GenlPacket NoData])
  -- ca me retourne un tas de paquet en fait ?
  _ <- mapM inspectPacket  pack

  -- putStrLn $show pack
  doDumpLoop sock

-- regarder dans query/joinMulticastGroup/recvOne
-- doDumpLoop / dumpGeneric
listenToEvents :: NetlinkSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents sock group = do
  -- joinMulticastGroup  returns IO ()
  joinMulticastGroup sock (grpId group) 
  doDumpLoop sock
  putStrLn $ "Joined grp " ++ (grpName group)

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  -- options <- execParser opts
  (sock, fid) <- makeMptcpSocket
  putStr "socket created: " >> print fid
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  mcastGroups <- getMulticastGroups sock fid

  -- listenToEvents sock 
  -- case mcastGroups of
  --   Just (_, grps) -> do
  -- let gid = getMulticast grp mcastGroups
  -- case gid of
  --   Just x -> joinMulticastGroup sock x >> doDumpLoop sock
  --   Nothing -> error "Could not find the specified multicast group"
  --   -- Nothing -> error "Could not find the specified family"

  -- case 
  -- map (\x -> joinMulticastGroup sock (grpId x)) mcastGroups
  -- mapM to ignore 
  mapM (\x -> listenToEvents sock x) mcastGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastGroups)
  putStrLn "finished"

    -- let flags   = foldr (.|.) 0 [fNLM_F_REQUEST]
    --     header  = Header eRTM_GETLINK flags 42 0
    --     message = LinkMsg 0 2 0
    --     attrs   = empty
    -- iface <- queryOne sock (Packet header message attrs)
    -- print (packetMessage iface)
    -- let attrs = packetAttributes iface
    -- print $ getLinkAddress attrs
    -- print $ getLinkBroadcast attrs
    -- print $ getLinkName attrs
    -- print $ getLinkMTU attrs
    -- print $ getLinkQDisc attrs
    -- print $ getLinkTXQLen attrs

-- dumpNumeric :: ByteString -> IO ()
-- dumpNumeric b = print $ unpack b
