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


#include "tcp_states.h"

-- eTCPStates
{#enum eTCPStates as TcpState {underscoreToCase} deriving (Eq, Show)#}


-- TODO
{#enum eTCPStates as IDiagExt {underscoreToCase} deriving (Eq, Show)#}
