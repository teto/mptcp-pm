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
-}
module Generated
where

import Foreign
import Foreign.C

#include "tcp_states.h"

#{enum TcpState, TcpState
  , tcp_established             = TCP_ESTABLISHED
  , tcp_listen                  = TCP_LISTEN
  , tcp_syn_recv               = TCP_SYN_RECV
  , tcp_syn_sent               = TCP_SYN_SENT
  }

