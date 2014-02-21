{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Net.SockDiag.Constants
Description : A module to bridge the haskell code to underlying C code
Portability : Linux

TODO might be best to just use the netlink script and adapt it
https://github.com/Ongy/netlink-hs/issues/7
-}
module Net.SockDiag.Constants
where

import Data.Word ()
import System.Linux.Netlink.Constants (MessageType)
import Data.Bits ()


-- from include/uapi/linux/inet_diag.h
#include <linux/inet_diag.h>

-- let it use Bits as well as fNLM_F_REQUEST so that I can chain them with .|.
-- , Bits TODO rename to eDiagExt ?
{#enum INET_DIAG_NONE as SockDiagExtensionId {underscoreToCase} deriving (Eq, Show)#}

#include <linux/sock_diag.h>

msgTypeSockDiag :: MessageType
msgTypeSockDiag  = {#const SOCK_DIAG_BY_FAMILY #}


-- {#enum AF_UNSPEC as eAddressFamily {underscoreToCase} deriving (Eq, Show)#}
-- TODO generate AF_INET (6) from include/linux/socket.h
-- IPPROTO_TCP defined in include/uapi/linux/in.h

-- TODO Convert struct ?
-- inet_diag_req_v2 

