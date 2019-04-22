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

-- {underscoreToCase}
{#enum MPTCP_ATTR_UNSPEC as MptcpAttr {} deriving (Eq, Show)#}

-- {underscoreToCase}
{#enum MPTCP_CMD_UNSPEC as MptcpGenlEvent {} deriving (Eq, Show)#}

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

#include "sock_diag.h"

-- cccaa 
msgTypeSockDiag :: MessageType
msgTypeSockDiag  = {#const SOCK_DIAG_BY_FAMILY #}

-- TODO generate AF_INET (6) from include/linux/socket.h
-- IPPROTO_TCP defined in include/uapi/linux/in.h

-- Convert struct ?
-- inet_diag_req_v2 
