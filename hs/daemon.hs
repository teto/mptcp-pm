-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString (ByteString, length, unpack, pack, concat)
import Data.Bits ((.|.))
import Data.Map (fromList, empty)
import Data.Char (ord)

import System.Linux.Netlink.Internal

main = do
    sock <- makeSocket

    let flags   = foldr (.|.) 0 [fNLM_F_REQUEST]
        header  = Header eRTM_GETLINK flags 42 0
        message = LinkMsg 0 2 0
        attrs   = empty
    iface <- queryOne sock (Packet header message attrs)
    print (packetMessage iface)
    let attrs = packetAttributes iface
    print $ getLinkAddress attrs
    print $ getLinkBroadcast attrs
    print $ getLinkName attrs
    print $ getLinkMTU attrs
    print $ getLinkQDisc attrs
    print $ getLinkTXQLen attrs

dumpNumeric :: ByteString -> IO ()
dumpNumeric b = print $ unpack b
