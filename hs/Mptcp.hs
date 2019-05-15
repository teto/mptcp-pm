{-|
Module      : MPTCP
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux

-}
module Mptcp
where

import Data.Word (Word8)

tcpMetricsGenlName :: String
tcpMetricsGenlName = "tcp_metrics"
tcpMetricsGenlVer :: Word8
tcpMetricsGenlVer = 1

