{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Net.Mptcp.Constants
Description : A module to bridge the haskell code to underlying C code

I consider this module internal.
The documentation may be a bit sparse.
Inspired by:
https://stackoverflow.com/questions/6689969/how-does-one-interface-with-a-c-enum-using-haskell-and-ffi

TODO might be best to just use the netlink script and adapt it
https://github.com/Ongy/netlink-hs/issues/7
-}
module Net.Mptcp.Constants
where

import Data.Word (Word8)
-- import System.Linux.Netlink.Constants (MessageType)
import Data.Bits ()

-- copy from include/net/tcp_states
#include <tcp_states.h>

-- For anonymous C enums, we can use , Bits
{#enum TCP_ESTABLISHED as TcpState {underscoreToCase} deriving (Eq, Show)#}

-- from include/uapi/linux/mptcp.h
#include <linux/mptcp.h>

-- {underscoreToCase}
-- add prefix = "e"
{#enum MPTCP_ATTR_UNSPEC as MptcpAttr {} omit (__MPTCP_ATTR_AFTER_LAST) deriving (Eq, Show)#}

-- {underscoreToCase}
-- can also be seen as a command
{#enum MPTCP_CMD_UNSPEC as MptcpGenlEvent {} deriving (Eq, Show)#}

-- |Generic netlink MPTCP version
mptcpGenlVer :: Word8
mptcpGenlVer = {#const MPTCP_GENL_VER #}

mptcpGenlName :: String
mptcpGenlName = {#const MPTCP_GENL_NAME #}
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = {#const MPTCP_GENL_CMD_GRP_NAME #}
mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName  = {#const MPTCP_GENL_EV_GRP_NAME #}

