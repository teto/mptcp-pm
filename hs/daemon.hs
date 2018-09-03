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

-- import Control.Applicative ((<$>))
-- import Control.Exception (throwIO)
-- import Data.ByteString (ByteString, length, unpack, pack, concat)
-- import Data.Bits ((.|.))
-- import Data.Map (fromList, empty)
-- import Data.Char (ord)

import System.Environment (getArgs)

import System.Linux.Netlink hiding (makeSocket)
import System.Linux.Netlink.GeNetlink (makeSocket)
import System.Linux.Netlink.Constants

import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word32, Word16, Word8)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
data MptcpSocket = NLS NetlinkSocket Word16


-- en fait y a 2 multicast groups
mptcp_genl_ev_grp_name = "mptcp_events"
mptcp_genl_cmd_grp_name = "mptcp_commands"

mptcp_genl_name="mptcp"

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
  fid <- getFamilyId sock mptcp_genl_name
  -- return $NLS sock fid
  return ( sock, fid)


-- 	err = genl_ctrl_grp_by_name(family, grp_name);
-- 	genl_family_put(family);
--
-- errout:
-- 	return err;

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  -- args <- getArgs
  options <- execParser opts
  (sock, fid) <- makeMptcpSocket
  putStrLn "socket created" >> print fid
  (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcp_genl_ev_grp_name
  putStrLn "finished"
    -- self.family_id = genl.genl_ctrl_resolve(self.sk, MPTCP_FAMILY_NAME)

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
