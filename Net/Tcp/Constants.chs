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
module Net.Tcp.Constants
where

import Data.Bits ()

-- copy from include/net/tcp_states
#include <tcp_states.h>

-- For anonymous C enums, we can use , Bits
{#enum TCP_ESTABLISHED as TcpState {underscoreToCase} deriving (Eq, Show)#}
