{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux


This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h

To interact
GENL_ADMIN_PERM
The operation requires the CAP_NET_ADMIN privilege
-}

-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)
import Options.Applicative hiding (value, ErrorMsg)
import qualified Options.Applicative (value)

import Data.Bits ((.|.))
import Foreign.C.Types (CInt)
import Foreign.C.Error
import System.Linux.Netlink hiding (makeSocket)
import System.Linux.Netlink (query, Packet(..))
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.Helpers
import System.Log.FastLogger

import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word8, Word16, Word32)
import Data.List (intercalate)
import Data.Binary.Get
import Data.Serialize.Put

import Data.ByteString as BS hiding (putStrLn, putStr, map, intercalate)
import qualified Data.ByteString.Lazy as BSL

import Debug.Trace
-- import Control.Exception


import qualified Data.Map as Map
-- import Data.ByteString.Char8 as C8 hiding (putStrLn, putStr)
 -- (unpack)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
data MptcpSocket = MptcpSocket NetlinkSocket Word16

-- inspired by NoData80211
data NoDataMptcp = NoDataMptcp deriving (Eq, Show)
instance Convertable NoDataMptcp where
  getPut _ = return ()
  getGet _ = return NoDataMptcp


-- data GenlData a = GenlData
--     {
--       genlDataHeader :: GenlHeader
--     , genlDataData   :: a
--     } deriving (Eq)
  --
-- type GenlPacket a = Packet (GenlData a)
-- type Mptcp
type MptcpPacket = Packet (GenlData NoDataMptcp)

-- inspired by CtrlPacket
  -- token :: MptcpToken
-- MptcpNewConnection
-- data MptcpPacket = MptcpPacket {
--       mptcpHeader     :: Header
--     , mptcpGeHeader   :: GenlHeader
--     , mptcpData   :: MptcpData
-- data MptcpData = MptcpData {
--     mptcpAttributes :: [MptcpAttr]
--   } deriving (Eq)

type MptcpToken = Word32
type LocId    = Word8
type MptcpFamily    = Word16

-- |typedef for messages send by this mdoule
-- type NL80211Packet = GenlPacket NoData80211
mptcpGenlVer :: Word8
mptcpGenlVer = 1

-- https://stackoverflow.com/questions/18606827/how-to-write-customised-show-function-in-haskell
-- TODO could use templateHaskell
-- TODO les generer via le netlink helper mkIncludeBlock
-- look at generate.hs
data MptcpGenlEvent =
  MPTCP_CMD_UNSPEC |

  MPTCP_EVENT_CREATED|
  MPTCP_EVENT_ESTABLISHED|
  MPTCP_EVENT_CLOSED|

  -- mptcp_nl_genl_announce
  MPTCP_CMD_ANNOUNCE|
  MPTCP_CMD_REMOVE|
  MPTCP_EVENT_ANNOUNCED|
  MPTCP_EVENT_REMOVED|

  MPTCP_CMD_SUB_CREATE|
  MPTCP_CMD_SUB_DESTROY|
  MPTCP_EVENT_SUB_ESTABLISHED|
  MPTCP_EVENT_SUB_CLOSED|

  MPTCP_CMD_SUB_PRIORITY|
  MPTCP_EVENT_SUB_PRIORITY|

  MPTCP_CMD_SET_FILTER|

  MPTCP_CMD_EXIST

  -- eventually derive "Bounded"
  deriving  (Enum, Show)

-- instance Show MptcpPacket where
--   show MPTCP_EVENT_CREATED = "MPTCP_EVENT_CREATED"
--   show x = x

-- dumpEnum
dumpCommand :: MptcpGenlEvent -> String
dumpCommand x = show x ++ " = " ++ show (fromEnum x)
-- dumpCommand MPTCP_CMD_UNSPEC  = " MPTCP_CMD_UNSPEC  0"
-- dumpCommand MPTCP_EVENT_SUB_ERROR = show MPTCP_EVENT_SUB_ERROR + show fromEnum MPTCP_EVENT_SUB_ERROR

dumpMptcpCommands :: MptcpGenlEvent -> String
dumpMptcpCommands MPTCP_CMD_EXIST = dumpCommand MPTCP_CMD_EXIST
dumpMptcpCommands x = dumpCommand x ++ "\n" ++ dumpMptcpCommands (succ x)

-- take inspiration from ctrlAttribute ?
-- data MptcpAttribute =
--   MPTCP_ATTR_TOKEN MptcpToken|
--   MPTCP_ATTR_FAMILY|
--   MPTCP_ATTR_LOC_ID|

-- data MptcpGenlGrp = MPTCP_GENL_EV_GRP_NAME | MPTCP_GENL_CMD_GRP_NAME
data MptcpAttr =
  MPTCP_ATTR_UNSPEC |
  MPTCP_ATTR_TOKEN |
  MPTCP_ATTR_FAMILY|
  MPTCP_ATTR_LOC_ID|
  MPTCP_ATTR_REM_ID|
  MPTCP_ATTR_SADDR4|
  MPTCP_ATTR_SADDR6|
  MPTCP_ATTR_DADDR4|
  MPTCP_ATTR_DADDR6|
  MPTCP_ATTR_SPORT|
  MPTCP_ATTR_DPORT|
  MPTCP_ATTR_BACKUP|
  MPTCP_ATTR_ERROR|
  -- used to filter events we are interested in
  MPTCP_ATTR_FLAGS|
  MPTCP_ATTR_TIMEOUT|
  MPTCP_ATTR_IF_IDX
  -- __MPTCP_ATTR_AFTER_LAST
  deriving  (Enum, Eq)

-- en fait y a 2 multicast groups
-- genl_multicast_group

mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName = "mptcp_events"
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = "mptcp_commands"
mptcpGenlName :: String
mptcpGenlName="mptcp"

data Sample = Sample
  { command      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> Options.Applicative.value 1
         <> metavar "INT" )


opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

-- MPTCP_GENL_CMD_GRP_NAME  = "mptcp_commands"
-- MPTCP_GENL_VERSION       = 1
-- MPTCP_FAMILY_NAME   = MPTCP_GENL_NAME = "mptcp"

-- NetlinkSocket
-- makeSocketGeneric
-- let active = take 1 args == ["--active"]

-- NetlinkSocket
-- mptcpNetlink :: IO ()
-- mptcpNetlink =
--   let
--     -- family_id = getFamilyId sock mptcpGenlEvGrpName
--     -- getFamilyId vs getMulticast
--     -- getFamilyIdS is the safe version returning a Maybe
--     -- joinMulticastGroup
--     genlFamilyId = makeSocket >>= \sock -> (getFamilyId sock mptcp_genl_name)
--     -- genlFamilyId = makeSocket >>= \sock -> (getMulticast sock mptcpGenlEvGrpName)
--   in
--     -- expects a word32
--     System.Linux.Netlink.GeNetlink.join sock familyId

-- inspired by makeNL80211Socket Create a 'NL80211Socket' this opens a genetlink
-- socket and gets the family id
-- TODO should record the token too
makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket = do
  sock <- makeSocket
-- getFamilyWithMulticasts
  res <- getFamilyIdS sock mptcpGenlName
  case res of
    Nothing -> error $ "Could not find family " ++ mptcpGenlName
    Just fid -> return  (MptcpSocket sock (trace ("family id"++ show fid ) fid))


-- tcp_metrics is a multicast group

-- errout:
-- 	return err;

-- inspectPacket :: GenlPacket NoData -> IO ()
-- inspectPacket = Prelude.putStrLn show
-- inspectPacket packet = Prelude.putStrLn $show packet

getRequestPacket :: Word16 -> MptcpGenlEvent -> Bool -> Attributes -> GenlPacket NoData
getRequestPacket fid cmd dump attrs =
  let
    -- The message type/ flag / sequence number / pid  (0 => from the kernel)
    myHeader = Header (fromIntegral fid) (flags .|. fNLM_F_ACK) 0 0
    -- GenlHeader <cmd> <version>
    geheader = GenlHeader word8Cmd mptcpGenlVer
    -- NLM_F_ACK
    -- https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/netlink.h#L54
    flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST
    word8Cmd = fromIntegral (fromEnum cmd) :: Word8
  in

    -- inspired by System/Linux/Netlink/GeNetlink/NL80211.hs
    -- Packet header (GenlData geheader NoData80211) attrs
    Packet myHeader (GenlData geheader NoData) attrs

  --   System.Linux.Netlink.ErrorMsg -> error "error msg"

toIpv4 :: ByteString -> String
toIpv4 val = Data.List.intercalate "." ( map show (BS.unpack val))


-- TODO convert port
-- t = BS.pack [ 232, 229 ]
-- Data.ByteString.Conversion.fromByteString t :: Maybe Data.Word.Word16

getPort :: ByteString -> Word16
getPort val =
  -- decode (BSL.fromStrict value) :: Word16
  runGet getWord16le (BSL.fromStrict val)

readToken :: Maybe ByteString -> MptcpToken
readToken maybeVal = case maybeVal of
  Nothing -> error "Missing token"
  Just val -> runGet getWord32le (BSL.fromStrict val)

readLocId :: Maybe ByteString -> LocId
readLocId maybeVal = case maybeVal of
  Nothing -> error "Missing locator id"
  Just val -> runGet getWord8 (BSL.fromStrict val)

dumpAttribute :: Int -> ByteString -> String
dumpAttribute attr value = let

  -- fromJust :: Maybe a -> a
  -- ip = C8.unpack value
  -- Data.ByteString.Char8
  -- ip =
  -- ip = case C8.unpack value of
  --   Nothing -> "no Ip"
  --   Just x -> show x

  attrStr = case toEnum (fromIntegral attr) of
      MPTCP_ATTR_UNSPEC -> "UNSPECIFIED"
      MPTCP_ATTR_TOKEN -> "token: " ++ show (readToken $ Just value)
      MPTCP_ATTR_FAMILY -> "family: " ++ show value
      MPTCP_ATTR_LOC_ID -> "Locator id: " ++ show (readLocId $ Just value)
      MPTCP_ATTR_REM_ID -> "Remote id: " ++ show value
      MPTCP_ATTR_SADDR4 -> "ipv4.src: " ++ toIpv4 value
      MPTCP_ATTR_SADDR6 -> "ipv6.src: " ++ show value
      MPTCP_ATTR_DADDR4 -> "ipv4.dest: " ++ toIpv4 value
      MPTCP_ATTR_DADDR6 -> "ipv6.dest: " ++ show value
      MPTCP_ATTR_SPORT -> "sport: " ++ show (getPort value)
      MPTCP_ATTR_DPORT -> "dport: " ++ show (getPort value)
      MPTCP_ATTR_BACKUP -> "backup" ++ show value
      MPTCP_ATTR_ERROR -> "Error: " ++ show value
      MPTCP_ATTR_FLAGS -> "Flags: " ++ show value
      MPTCP_ATTR_TIMEOUT -> "timeout: " ++ show value
      MPTCP_ATTR_IF_IDX -> "ifId: " ++ show value

      -- _ -> "unhandled case"
  in
    attrStr ++ "\n"



-- type Attributes = Map Int ByteString
-- https://lotz84.github.io/haskellbyexample/ex/maps
-- the library contains showAttrs / showNLAttrs
showAttributes :: Attributes -> String
showAttributes attrs =
  let
    -- f k v = [show k, " = ", show v]
    -- mapped = Map.mapWithKey f attrs
    mapped = Map.foldrWithKey (\k v -> (dumpAttribute k v ++ ) ) " " attrs
  in
    -- putStrLn $ intercalate "," $ mapped
    mapped



-- - MPTCP_CMD_SUB_CREATE: token, family, loc_id, rem_id, [saddr4 | saddr6,
--                         daddr4 | daddr6, dport [, sport, backup, if_idx]]
--     Create a new subflow.
--  one can look at  Route.hs for "putMessage"
-- putMessage :: Message -> Put
-- putMessage (NLinkMsg ty idx flags) = do
--     p8 eAF_UNSPEC >> p8 0
--     p16 (fromIntegral ty)
--     p32 idx
--     p32 flags
--     p32 0xFFFFFFFF

createNewSubflow :: MptcpSocket -> MptcpToken -> Attributes -> IO ()
createNewSubflow (MptcpSocket socket fid) token attrs = let
  localId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_LOC_ID) attrs
  remoteId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_REM_ID) attrs
  in
  putStrLn "createNewSubflow TODO"
  -- if (!info->attrs[MPTCP_ATTR_TOKEN] || !info->attrs[MPTCP_ATTR_FAMILY] ||
  --     !info->attrs[MPTCP_ATTR_LOC_ID] || !info->attrs[MPTCP_ATTR_REM_ID])


-- removeSubflow :: MptcpSocket -> MptcpToken -> LocId -> IO [GenlPacket NoData]
-- removeSubflow (MptcpSocket socket fid) token locId = let

-- I want to override the GenlHeader version
newtype GenlHeaderMptcp = GenlHeaderMptcp GenlHeader
instance Show GenlHeaderMptcp where
  show (GenlHeaderMptcp (GenlHeader cmd ver)) =
    "Header: Cmd = " ++ show cmd ++ ", Version: " ++ show ver ++ "\n"


removeLocId :: MptcpSocket -> MptcpToken -> LocId -> IO [GenlPacket NoData]
removeLocId (MptcpSocket socket fid) token locId = let
    m0 = Map.empty
    m1 = Map.insert (fromEnum MPTCP_ATTR_TOKEN) (convertToken token) m0
    m2 = Map.insert (fromEnum MPTCP_ATTR_LOC_ID) (convertLocId locId) m1
    -- MPTCP_ATTR_IF_IDX and MPTCP_ATTR_BACKUP should be optional
    --
    attributes = m2
    -- intCmd = fromEnum cmd
    cmd = MPTCP_CMD_REMOVE
    pkt = getRequestPacket fid cmd False attributes
  in
    putStrLn ("Remove subflow " ++ showAttributes attributes)
      >> query socket pkt


checkIfSocketExists :: MptcpSocket -> MptcpToken  -> IO [GenlPacket NoData]
checkIfSocketExists (MptcpSocket socket fid) token = let
    attributes = Map.insert (fromEnum MPTCP_ATTR_TOKEN) (convertToken token) Map.empty
    pkt = getRequestPacket fid cmd False attributes
    cmd = MPTCP_CMD_EXIST
  in
    putStrLn ("Checking token exists\n" ++ showAttributes attributes ++ showPacket pkt)
      -- >> putStrLn $
      >> query socket pkt

--announceSubflow :: MptcpSocket -> MptcpToken -> IO [GenlPacket NoData]
--announceSubflow (MptcpSocket socket fid) token = let
--    m0 = Map.empty
--    -- TODO I should add the command at some point here
--    m1 = Map.insert MPTCP_ATTR_TOKEN (convertToken token) m0
--    -- the loc id should exist :/
--    m2 = Map.insert MPTCP_ATTR_LOC_ID (convertToken token) m1
--    -- MPTCP_ATTR_IF_IDX and MPTCP_ATTR_BACKUP should be optional
--    --
--    attributes = m1
--    intCmd = fromEnum cmd
--    cmd = MPTCP_CMD_ANNOUNCE
--    pkt = getRequestPacket fid cmd False attributes
--  in
--    query socket pkt


-- TODO here I could helpers from the netlink library
convertLocId :: Word8 -> ByteString
convertLocId val =
  runPut $ putWord8 val

convertToken :: Word32 -> ByteString
convertToken val =
  runPut $ putWord32le val

-- need to prepare a request
-- type GenlPacket a = Packet (GenlData a)
resetTheConnection :: MptcpSocket -> Attributes -> IO [GenlPacket NoData]
resetTheConnection (MptcpSocket socket fid) receivedAttributes = let
    m0 = Map.empty
    m1 = Map.insert intCmd (convertToken token) m0
    token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) receivedAttributes
    attributes = m1
    intCmd = fromEnum cmd
    cmd = MPTCP_CMD_REMOVE

    -- getRequestPacket fid cmd dump attrs =
    -- getRequestPacket :: Word16 -> MptcpGenlEvent -> Bool -> Attributes -> GenlPacket NoData
    pkt = getRequestPacket fid cmd False attributes
  in
    -- GenlPacket a with a acting as genlDataData
    -- query or queryOne
    -- System.Linux.Netlink query :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO [Packet a]
    query socket pkt

inspectAnswers :: [GenlPacket NoData] -> IO ()
inspectAnswers packets = do
  mapM_ inspectAnswer packets
  putStrLn "Finished inspecting answers"

-- showPacketCustom :: GenlPacket NoData -> String
-- showPacketCustom pkt = let
--   hdr = (genlDataHeader pkt )
--   in showPacket pkt

showHeaderCustom :: GenlHeader -> String
showHeaderCustom hdr = show hdr

inspectAnswer :: GenlPacket NoData -> IO ()
-- inspectAnswer packet = putStrLn $ "Inspecting answer:\n" ++ showPacket packet
-- (GenlData NoData)
-- inspectAnswer (Packet hdr (GenlData ghdr NoData) attributes) = putStrLn $ "Inspecting answer:\n"
inspectAnswer (Packet _ (GenlData hdr NoData) attributes) = let
    cmd = genlCmd hdr
  in
    putStrLn $ "Inspecting answer custom:\n" ++ showHeaderCustom hdr
            ++ "Supposing it's a mptcp command" ++ dumpCommand ( toEnum $ fromIntegral cmd)

inspectAnswer pkt = putStrLn $ "Inspecting answer:\n" ++ showPacket pkt

onNewConnection :: MptcpSocket -> Attributes -> IO ()
onNewConnection socket attributes = do
    let token = readToken $ Map.lookup (fromEnum MPTCP_ATTR_TOKEN) attributes
    let locId = readLocId $ Map.lookup (fromEnum MPTCP_ATTR_LOC_ID) attributes
    let answer = "e"
    -- answer <-  Prelude.getLine
    -- putStrLn "What do you want to do ? (c.reate subflow, d.elete connection, r.eset connection)"
    -- announceSubflow socket token >>= inspectAnswers >> putStrLn "Finished announcing"
    case answer of
      "c" -> do
        putStrLn "Creating new subflow !!"
        createNewSubflow socket token attributes
        return ()
      "d" -> putStrLn "Not implemented"
        -- removeSubflow socket token locId >>= inspectAnswers >> putStrLn "Finished announcing"
      "e" -> putStrLn "check for existence" >>
          checkIfSocketExists socket token >>= inspectAnswers
          -- checkIfSocketExists socket token >>= (dispatchPacket socket)
          -- return ()
      "r" -> putStrLn "Reset the connection" >>
        -- TODO expects token
        -- TODO discard result
          -- resetTheConnection socket token >>= inspectAnswers >>
          putStrLn "Finished resetting"
      _ -> onNewConnection socket attributes
    return ()


-- type GenlPacket a = Packet (GenlData a)
-- overloaded dispatching inspired by System/Linux/Netlink.hs:showPacket
-- dispatchPacket :: MptcpSocket -> Packet -> IO ()
-- dispatchPacket sock (System.Linux.Netlink.ErrorMsg hdr code packet) = do
--   putStrLn "a netlink error happened"
-- dispatchPacket sock (DoneMsg hdr) = do
--   putStrLn "Done Msg"

-- type GenlPacket a = Packet (GenlData a)
-- type MptcpPacket = GenlPacket NoDataMptcp
-- dispatchPacket :: MptcpSocket -> GenlPacket NoData -> IO ()
-- dispatchPacket sock (Packet hdr (GenlData NoData) attributes) = do
--     -- cmd = genlCmd genlHeader
--     putStrLn "Genldata"

dispatchPacket :: MptcpSocket -> MptcpPacket -> IO ()
dispatchPacket sock (Packet hdr (GenlData genlHeader NoDataMptcp) attributes) = let
    cmd = genlCmd genlHeader
  in
    case toEnum (fromIntegral cmd) of
      MPTCP_EVENT_CREATED -> do
            putStrLn $ "Connection created !!\n" ++ showAttributes attributes
            onNewConnection sock attributes
      MPTCP_EVENT_ESTABLISHED -> putStrLn "Connection established !"
      MPTCP_EVENT_CLOSED -> putStrLn "Connection closed"
      MPTCP_EVENT_SUB_ESTABLISHED -> putStrLn "Subflow established"
      MPTCP_EVENT_SUB_CLOSED -> putStrLn "Subflow closed"
      MPTCP_CMD_EXIST -> putStrLn "this token exists"
      _ -> putStrLn "undefined event !!"

-- dispatchPacket sock (ErrorMsg err) attributes) = let
dispatchPacket sock (DoneMsg err) =
  putStrLn "Done msg"


dispatchPacket sock (ErrorMsg hdr errCode errPacket) =
  putStrLn $ "Error msg of type " ++ showErrCode errCode ++ " Packet content:\n" ++ show errPacket

-- /usr/include/asm/errno.h
showErrCode :: CInt -> String
showErrCode err
  | Errno err == ePERM = "EPERM"
  | Errno err == eOK = "EOK"
  | otherwise = show err

-- showErrCode err = case err of
-- -- show err
--   ePERM -> "EPERM"
--   eNOTCONN -> "NOT connected"


inspectResult :: MptcpSocket -> Either String MptcpPacket -> IO()
inspectResult mptcpSocket result =  case result of
    Left ex -> putStrLn $ "An error in parsing happened" ++ show ex
    Right myPack -> dispatchPacket mptcpSocket myPack >> putStrLn "Valid packet"

-- CtrlAttrMcastGroup
-- copied from utils/GenlInfo.hs
doDumpLoop :: MptcpSocket -> IO ()
doDumpLoop (MptcpSocket simpleSock fid) = do
  -- putStrLn $show pack
  putStrLn "doDumpLoop"
  -- TODO do less filtering here ?
  -- myPack <- trace "recvOne" (recvOne simpleSock :: IO [GenlPacket NoData])

  -- inspired by https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
  -- to work around "user error (too few bytes       From: demandInput     )"
  --
  -- [Either String (Packet a)]
  -- (Convertable a, Eq a, Show a) =>
  results <- recvOne' simpleSock ::  IO [Either String MptcpPacket]

  mapM_ (inspectResult mptcpSocket) results

  -- ca me retourne un tas de paquet en fait ?
  -- For a version that ignores the results see mapM_.
  -- _ <- mapM_ (dispatchPacket mptcpSocket) myPack
  -- _ <- mapM inspectPacket  pack
  doDumpLoop mptcpSocket
  where
    mptcpSocket = MptcpSocket simpleSock fid

-- regarder dans query/joinMulticastGroup/recvOne
-- doDumpLoop / dumpGeneric
listenToEvents :: MptcpSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents (MptcpSocket sock fid) my_group = do
  -- joinMulticastGroup  returns IO ()
  -- TODO should check it works correctly !
  joinMulticastGroup sock (grpId my_group)
  putStrLn $ "Joined grp " ++ grpName my_group
  doDumpLoop mptcpSocket
  where
    mptcpSocket = MptcpSocket sock fid


createLogger :: IO LoggerSet
createLogger = newStdoutLoggerSet defaultBufSize


-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  options <- execParser opts
  logger <- createLogger
  pushLogStr logger (toLogStr "ok")
  putStrLn "dumping important values:"
  -- putStrLn $ "buffer size " ++ show bufferSize
  putStrLn $ "RESET" ++ show MPTCP_CMD_REMOVE
  putStrLn $ dumpMptcpCommands MPTCP_CMD_UNSPEC
  putStrLn "Creating MPTCP netlink socket..."
  (MptcpSocket sock  fid) <- makeMptcpSocket
  putStr "socket created. Family id " >> print fid
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  mcastGroups <- getMulticastGroups sock fid
  -- IO [CtrlAttrMcastGroup]
  mapM_ print mcastGroups

  mapM_ (listenToEvents (MptcpSocket sock fid)) mcastGroups
  -- putStrLn $ " Groups: " ++ unwords ( map grpName mcastGroups)
  putStrLn "finished"

    -- let flags   = foldr (.|.) 0 [fNLM_F_REQUEST]
    --     header  = Header eRTM_GETLINK flags 42 0
    --     message = LinkMsg 0 2 0
    --     attrs   = empty
    -- iface <- queryOne sock (Packet header message attrs)
    -- print (packetMessage iface)
    -- let attrs = packetAttributes iface
    -- print $ getLinkAddress attrs
    -- print $ getLinkBroadcast attr

-- dumpNumeric :: ByteString -> IO ()
-- dumpNumeric b = print $ unpack b
