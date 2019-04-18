{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Generated
Description : A module to bridge the haskell code to underlying C code
Maintainer  : ongy
Stability   : testing
Portability : Linux

I consider this module internal.
The documentation may be a bit sparse.
Inspired by:
https://stackoverflow.com/questions/6689969/how-does-one-interface-with-a-c-enum-using-haskell-and-ffi

TODO might be best to just use the netlink script and adapt it
https://github.com/Ongy/netlink-hs/issues/7
-}
module Generated
where

import Data.Word (Word8)


-- copy from include/net/tcp_states
#include "tcp_states.h"

-- For anonymous C enums, we can use 
{#enum TCP_ESTABLISHED as TcpState {underscoreToCase} deriving (Eq, Show)#}

-- copy from include/uapi/linux/mptcp.h
#include "mptcp.h"

{#enum MPTCP_ATTR_UNSPEC as MptcpAttr {underscoreToCase} deriving (Eq, Show)#}
-- TODO reuse
-- #define MPTCP_GENL_NAME		"mptcp"
-- #define MPTCP_GENL_EV_GRP_NAME	"mptcp_events"
-- #define MPTCP_GENL_CMD_GRP_NAME "mptcp_commands"
-- #define MPTCP_GENL_VER		0x1
mptcpGenlVer :: Word8
mptcpGenlVer = {#const MPTCP_GENL_VER #}

mptcpGenlName :: String
mptcpGenlName = {#const MPTCP_GENL_NAME #}
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = {#const MPTCP_GENL_CMD_GRP_NAME #}
mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName  = {#const MPTCP_GENL_EV_GRP_NAME #}

-- copy from include/uapi/linux/inet_diag.h
#include "inet_diag.h"

{#enum INET_DIAG_NONE as IDiagExt {underscoreToCase} deriving (Eq, Show)#}

-- Convert struct ?
-- inet_diag_req_v2 
