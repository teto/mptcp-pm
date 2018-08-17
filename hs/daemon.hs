-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)

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


mptcpGenlEvGrpName = "mptcp_events"

-- MPTCP_GENL_CMD_GRP_NAME  = "mptcp_commands"
-- MPTCP_GENL_VERSION       = 1
-- MPTCP_FAMILY_NAME   = MPTCP_GENL_NAME = "mptcp"

-- NetlinkSocket
-- makeSocketGeneric
-- let active = take 1 args == ["--active"] 

mptcpNetlink :: IO NetlinkSocket
mptcpNetlink = 
  let family_id = getFamilyId mptcpGenlEvGrpName
          in
          makeSocket >>= \sock ->
                  joinMulticastGroup sock mptcpGenlEvGrpName
                  >> pure sock


-- 	err = genl_ctrl_grp_by_name(family, grp_name);
-- 	genl_family_put(family);
-- errout:
-- 	return err;

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  -- args <- getArgs
  sock <- mptcpNetlink
  putStrLn "shel"

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
