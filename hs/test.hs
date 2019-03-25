import Prelude hiding (length, concat)
import Options.Applicative hiding (value)
import qualified Options.Applicative (value)

import Data.Bits ((.|.))
import System.Linux.Netlink hiding (makeSocket)
import System.Linux.Netlink (query, bufferSize)
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants

import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word8, Word16, Word32)
import Data.List (intercalate)
import Data.Binary.Get
import Data.Serialize.Put

import Data.ByteString as BS hiding (putStrLn, putStr, map, intercalate)
import qualified Data.ByteString.Lazy as BSL

import Debug.Trace
import Control.Exception

data MptcpSocket = MptcpSocket NetlinkSocket Word16

mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName = "mptcp_events"
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = "mptcp_commands"
mptcpGenlName :: String
mptcpGenlName="mptcp"

makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket = do
  sock <- makeSocket
  putStrLn "socket created"
  res <- getFamilyIdS sock mptcpGenlName
  case res of
    Nothing -> error $ "Could not find family " ++ mptcpGenlName
    Just fid -> return  (MptcpSocket sock (trace ("family id"++ show fid ) fid))


main :: IO ()
main = do
  putStrLn "making socket"
  makeMptcpSocket
  putStrLn "finished"
