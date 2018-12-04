{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of mptcp netlink path manager
Maintainer  : matt
Stability   : testing
Portability : Linux


This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h
-}

-- !/usr/bin/env nix-shell
-- !nix-shell ../shell-haskell.nix -i ghc
module Main where

import Prelude hiding (length, concat)
import Options.Applicative
-- import Data.Semigroup ((<>))

-- import Data.ByteString (ByteString, length, unpack, pack, concat)

-- import System.Environment (getArgs)

import System.Linux.Netlink hiding (makeSocket)
-- (makeSocket, GenlPacket, getGenlHeader )
import System.Linux.Netlink.GeNetlink
-- import System.Linux.Netlink.Constants
-- import System.Linux.Netlink.Utils

import System.Linux.Netlink.GeNetlink.Control as C
import Data.Word (Word16)
import Data.List (intercalate)
-- import Data.String
import Data.ByteString hiding (putStrLn, putStr)
import qualified Data.Map as Map

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
-- data MptcpSocket = NLS NetlinkSocket Word16


-- data Command = MPTCP_CMD_ANNOUNCE | MPTCP_CMD_REMOVE | MPTCP_CMD_SUB_CREATE 
--   deriving  (Enum)


--  as an alternative to enums
-- newtype MessageType = MessageType Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

-- showMessageType :: (Num a) => (Show a) => (Eq a) => a -> String
-- showMessageType 1 = "NLMSG_NOOP"
-- showMessageType 2 = "NLMSG_ERROR"
-- showMessageType 94 = "RTM_GETSTATS"
-- showMessageType i = "MessageType #" ++ (show i)

data MptcpGenlEvent = MPTCP_CMD_UNSPEC |
  MPTCP_EVENT_CREATED|
  MPTCP_EVENT_ESTABLISHED|
  MPTCP_EVENT_CLOSED|
  MPTCP_CMD_ANNOUNCE|
  MPTCP_CMD_REMOVE|
  MPTCP_EVENT_ANNOUNCED|
  MPTCP_EVENT_REMOVED|
  MPTCP_CMD_SUB_CREATE|
  MPTCP_CMD_SUB_DESTROY|
  MPTCP_EVENT_SUB_CREATED|
  MPTCP_EVENT_SUB_ESTABLISHED|
  MPTCP_EVENT_SUB_CLOSED|

  MPTCP_CMD_SUB_PRIORITY|
  MPTCP_EVENT_SUB_PRIORITY|

  MPTCP_CMD_RESET|
  MPTCP_CMD_SET_FILTER|
  MPTCP_CMD_SUB_TIMEOUT|
  MPTCP_CMD_DUMP|

  MPTCP_CMD_EXIST|
  MPTCP_EVENT_SUB_ERROR
  deriving  (Enum)



-- data MptcpGenlGrp = MPTCP_GENL_EV_GRP_NAME | MPTCP_GENL_CMD_GRP_NAME
data MptcpAttr = 
  MPTCP_ATTR_TOKEN|
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
  MPTCP_ATTR_FLAGS| 
  MPTCP_ATTR_TIMEOUT| 

  MPTCP_ATTR_IF_IDX
  deriving  (Enum)

-- en fait y a 2 multicast groups
-- genl_multicast_group 

mptcpGenlEvGrpName :: String
mptcpGenlEvGrpName = "mptcp_events"
mptcpGenlCmdGrpName :: String
mptcpGenlCmdGrpName = "mptcp_commands"
mptcpGenlName :: String
mptcpGenlName="mptcp"

data Sample = Sample
  { hello      :: String
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
         <> value 1
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
-- makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket :: IO (NetlinkSocket, Word16)
makeMptcpSocket = do
  sock <- makeSocket
  fid <- getFamilyId sock mptcpGenlName
  -- return $NLS sock fid
  return ( sock, fid)


-- 	err = genl_ctrl_grp_by_name(family, grp_name);
-- 	genl_family_put(family);
--
-- errout:
-- 	return err;

inspectPacket :: GenlPacket NoData -> IO ()
inspectPacket packet = do

  Prelude.putStrLn $show packet
  -- return IO
  -- case pack of
  --   DoneMsg -> putStrLn "Done Msg"
  --   System.Linux.Netlink.ErrorMsg -> error "error msg"
  --   Packet -> putStrLn $ packetHeader pack


dumpAttribute :: Int -> ByteString -> String
dumpAttribute attr value = 
    -- case (toEnum (fromIntegral cmd)) of
    case (attr) of
      MPTCP_ATTR_TOKEN -> show attr ++ "TOKEN"
      MPTCP_ATTR_IF_IDX -> show attr ++ "ifId"
      MPTCP_ATTR_TIMEOUT -> show attr ++ show value;


    

--
-- import Data.Map (Map, keys)
-- type Attributes = Map Int ByteString
-- https://lotz84.github.io/haskellbyexample/ex/maps
showAttributes :: Attributes -> String
showAttributes attrs =
  let 
    -- f k v = [show k, " = ", show v]
    -- mapped = Map.mapWithKey f attrs
  -- ++ "=" ++ (show v))
    mapped = Map.foldrWithKey (\k v -> (dumpAttribute k v ++ ) ) " " attrs
  in 
    -- putStrLn $ intercalate "," $ mapped
    -- "toto"
    mapped
  -- map (\x -> 
  -- putStrLn

-- return une fonction ?
-- genlVersion
-- type GenlPacket a = Packet (GenlData a)
dispatchPacket :: GenlPacket NoData -> IO ()
dispatchPacket packet = let 
    -- todo not the good call
    -- genlHeader = packetHeader packet
    temp_data = packetCustom packet
    genl_header = genlDataHeader temp_data
    attributes = packetAttributes packet
    genl_data = genlDataData temp_data
    -- header = packetHeader packet
    -- `cmd` of type Word8
    cmd = genlCmd genl_header
    version = genlVersion genl_header
  in
    -- expects an Int
    case (toEnum (fromIntegral cmd)) of
    -- case (toEnum 2) of
      MPTCP_EVENT_CREATED -> putStrLn $ "Connection created !!" ++ (showAttributes attributes)
      MPTCP_EVENT_ESTABLISHED -> putStrLn "Connection established !"
      MPTCP_EVENT_CLOSED -> putStrLn "Connection closed"
      MPTCP_EVENT_SUB_CREATED -> putStrLn "Subflow created"
      MPTCP_EVENT_SUB_ESTABLISHED -> putStrLn "Subflow established"
      MPTCP_EVENT_SUB_CLOSED -> putStrLn "Subflow closed"
      _ -> putStrLn "undefined event !!"

      -- putStrLn $show genl_data
    -- case (messageType header) of
    --   MPTCP_EVENT_CREATED -> putStrLn "Connection created !!"
    --   MPTCP_CMD_UNSPEC -> putStrLn "UNKNOWN COMMAND ERROR "

-- CtrlAttrMcastGroup
-- copied from utils/GenlInfo.hs
doDumpLoop :: NetlinkSocket -> IO ()
doDumpLoop sock = do
  -- type GenlPacket a = Packet (GenlData a) 
  -- with data GenlData A wrapper around GenlHeader
  -- genlDataHeader
  pack <- (recvOne sock :: IO [GenlPacket NoData])
  -- ca me retourne un tas de paquet en fait ?
  -- _ <- pack.genlCmd
  _ <- mapM dispatchPacket  pack

  -- what does it do already ?
  -- _ <- mapM inspectPacket  pack

  -- putStrLn $show pack
  doDumpLoop sock

-- regarder dans query/joinMulticastGroup/recvOne
--
-- doDumpLoop / dumpGeneric
listenToEvents :: NetlinkSocket -> CtrlAttrMcastGroup -> IO ()
listenToEvents sock group = do
  -- joinMulticastGroup  returns IO ()
  -- TODO should check it works correctly !
  joinMulticastGroup sock (grpId group)
  putStrLn $ "Joined grp " ++ (grpName group)
  doDumpLoop sock

-- s'inspirer de
-- https://github.com/vdorr/linux-live-netinfo/blob/24ead3dd84d6847483aed206ec4b0e001bfade02/System/Linux/NetInfo.hs
main :: IO ()
main = do
  -- options <- execParser opts
  (sock, fid) <- makeMptcpSocket
  putStr "socket created: " >> print fid
  -- (mid, mcastGroup ) <- getFamilyWithMulticasts sock mptcpGenlEvGrpName
  mcastGroups <- getMulticastGroups sock fid

  -- listenToEvents sock 
  -- case mcastGroups of
  --   Just (_, grps) -> do
  -- let gid = getMulticast grp mcastGroups
  -- case gid of
  --   Just x -> joinMulticastGroup sock x >> doDumpLoop sock
  --   Nothing -> error "Could not find the specified multicast group"
  --   -- Nothing -> error "Could not find the specified family"

  -- case 
  -- map (\x -> joinMulticastGroup sock (grpId x)) mcastGroups
  -- mapM to ignore 
  _ <- mapM (\x -> listenToEvents sock x) mcastGroups
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
    -- print $ getLinkBroadcast attrs
    -- print $ getLinkName attrs
    -- print $ getLinkMTU attrs
    -- print $ getLinkQDisc attrs
    -- print $ getLinkTXQLen attrs

-- dumpNumeric :: ByteString -> IO ()
-- dumpNumeric b = print $ unpack b
