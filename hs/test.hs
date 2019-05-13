import Prelude hiding (length, concat)
-- import Options.Applicative hiding (value)
-- import qualified Options.Applicative (value)

import Generated
import Data.Bits ()
import Data.Bits (( .|.), shiftL)
-- import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, bufferSize)
-- import System.Linux.Netlink.GeNetlink
-- import System.Linux.Netlink.Constants

-- import System.Linux.Netlink.GeNetlink.Control as C
-- import Data.Word (Word8, Word16, Word32)
-- import Data.List (intercalate)
-- import Data.Serialize.Put

import IDiag

-- import Data.ByteString as BS hiding (putStrLn, putStr, map, intercalate)
-- import qualified Data.ByteString.Lazy as BSL

-- import Debug.Trace
-- import Control.Exception

-- data MptcpSocket = MptcpSocket NetlinkSocket Word16

-- mptcpGenlEvGrpName :: String
-- mptcpGenlEvGrpName = "mptcp_events"
-- mptcpGenlCmdGrpName :: String
-- mptcpGenlCmdGrpName = "mptcp_commands"
-- mptcpGenlName :: String
-- mptcpGenlName="mptcp"

-- makeMptcpSocket :: IO MptcpSocket
-- makeMptcpSocket = do
--   sock <- makeSocket
--   putStrLn "socket created"
--   res <- getFamilyIdS sock mptcpGenlName
--   case res of
--     Nothing -> error $ "Could not find family " ++ mptcpGenlName
--     Just fid -> return  (MptcpSocket sock (trace ("family id"++ show fid ) fid))


-- instance Bits TcpState where
--   shiftL x = shiftL (fromEnum x - 1)


-- in fact it's 1 << fromEnum TcpListen)
-- (fromEnum TcpListen) .|. (fromEnum TcpEstablished)

-- #define SS_ALL ((1 << SS_MAX) - 1)
-- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
-- #define TIPC_SS_CONN ((1<<SS_ESTABLISHED)|(1<<SS_LISTEN)|(1<<SS_CLOSE))

main :: IO ()
main = let 
    -- TODO test with shiftL
    toto = (fromEnum TcpListen) .|. (fromEnum TcpEstablished)
    zut = shiftL (fromEnum TcpListen)
  in do
  putStrLn $ "TcpListen =" ++ show (tcpStatesToWord [TcpListen])
  putStrLn $ "TcpEstablished =" ++ show (tcpStatesToWord [TcpEstablished])
  putStrLn $ "combo !!" ++ show (tcpStatesToWord [TcpEstablished, TcpListen])
  putStrLn "finished"
