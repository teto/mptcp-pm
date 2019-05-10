import Prelude hiding (length, concat)
-- import Options.Applicative hiding (value)
-- import qualified Options.Applicative (value)

-- import Generated
import Data.Bits (( .|.))
-- import System.Linux.Netlink hiding (makeSocket)
-- import System.Linux.Netlink (query, bufferSize)
-- import System.Linux.Netlink.GeNetlink
-- import System.Linux.Netlink.Constants

-- import System.Linux.Netlink.GeNetlink.Control as C
-- import Data.Word (Word8, Word16, Word32)
-- import Data.List (intercalate)
-- import Data.Serialize.Put

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

-- For anonymous C enums, we can use , Bits
data TcpState = TcpEstablished
              | TcpSynSent
              | TcpSynRecv
              | TcpFinWait1
              | TcpFinWait2
              | TcpTimeWait
              | TcpClose
              | TcpCloseWait
              | TcpLastAck
              | TcpListen
              | TcpClosing
              | TcpNewSynRecv
              | TcpMaxStates
  deriving (Eq,Show)
instance Enum TcpState where
  succ TcpEstablished = TcpSynSent
  succ TcpSynSent = TcpSynRecv
  succ TcpSynRecv = TcpFinWait1
  succ TcpFinWait1 = TcpFinWait2
  succ TcpFinWait2 = TcpTimeWait
  succ TcpTimeWait = TcpClose
  succ TcpClose = TcpCloseWait
  succ TcpCloseWait = TcpLastAck
  succ TcpLastAck = TcpListen
  succ TcpListen = TcpClosing
  succ TcpClosing = TcpNewSynRecv
  succ TcpNewSynRecv = TcpMaxStates
  succ TcpMaxStates = error "TcpState.succ: TcpMaxStates has no successor"

  pred TcpSynSent = TcpEstablished
  pred TcpSynRecv = TcpSynSent
  pred TcpFinWait1 = TcpSynRecv
  pred TcpFinWait2 = TcpFinWait1
  pred TcpTimeWait = TcpFinWait2
  pred TcpClose = TcpTimeWait
  pred TcpCloseWait = TcpClose
  pred TcpLastAck = TcpCloseWait
  pred TcpListen = TcpLastAck
  pred TcpClosing = TcpListen
  pred TcpNewSynRecv = TcpClosing
  pred TcpMaxStates = TcpNewSynRecv
  pred TcpEstablished = error "TcpState.pred: TcpEstablished has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from TcpMaxStates

  fromEnum TcpEstablished = 1
  fromEnum TcpSynSent = 2
  fromEnum TcpSynRecv = 3
  fromEnum TcpFinWait1 = 4
  fromEnum TcpFinWait2 = 5
  fromEnum TcpTimeWait = 6
  fromEnum TcpClose = 7
  fromEnum TcpCloseWait = 8
  fromEnum TcpLastAck = 9
  fromEnum TcpListen = 10
  fromEnum TcpClosing = 11
  fromEnum TcpNewSynRecv = 12
  fromEnum TcpMaxStates = 13

  toEnum 1 = TcpEstablished
  toEnum 2 = TcpSynSent
  toEnum 3 = TcpSynRecv
  toEnum 4 = TcpFinWait1
  toEnum 5 = TcpFinWait2
  toEnum 6 = TcpTimeWait
  toEnum 7 = TcpClose
  toEnum 8 = TcpCloseWait
  toEnum 9 = TcpLastAck
  toEnum 10 = TcpListen
  toEnum 11 = TcpClosing
  toEnum 12 = TcpNewSynRecv
  toEnum 13 = TcpMaxStates
  toEnum unmatched = error ("TcpState.toEnum: Cannot match " ++ show unmatched)


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
  putStrLn "making socket"
  -- makeMptcpSocket
  putStrLn $ "TcpListen =" ++ show (fromEnum TcpListen)
  putStrLn $ "TcpEstablished =" ++ show (fromEnum TcpEstablished)
  putStrLn $ show toto
  putStrLn "finished"
