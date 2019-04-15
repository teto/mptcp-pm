{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : System.Linux.Netlink.C
Description : A module to bridge the haskell code to underlying C code
Maintainer  : ongy
Stability   : testing
Portability : Linux

I consider this module internal.
The documentation may be a bit sparse.
-}
module Generated
    ( makeSocket
    , makeSocketGeneric
    )
where

#{enum TcpState, TcpState
  , tcp_established             = TCP_ESTABLISHED
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }
#include "tcp_states.h"

