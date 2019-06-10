{-| 
https://stackoverflow.com/questions/32913552/why-does-my-hunit-test-suite-pass-when-my-tests-fail
-}
module Main where

import System.Exit
import Test.HUnit
import Generated
import IDiag
import Net.Mptcp
import Net.IP
import Net.IPv4 (localhost)

-- main = do
--     putStrLn "This test always fails!"
--     exitFailure

testEmpty = TestCase $ assertEqual
  "Check"
  1
  ( enumsToWord [TcpEstablished] )

testCombo = TestCase $ assertEqual
  "Check"
  513
  ( enumsToWord [TcpEstablished, TcpListen] )

testComboReverse = TestCase $ assertEqual
  "Check"
  513
  ( enumsToWord [TcpEstablished, TcpListen] )


iperfConnection = TcpConnection {
        srcIp = fromIPv4 localhost
        , dstIp = fromIPv4 localhost
        , srcPort = 5000
        , dstPort = 1000
        -- placeholder values
        , priority = Nothing
        , subflowInterface = Nothing
        , localId = 0
        , remoteId = 0
        , inetFamily = 2
    }

filteredConnections :: [TcpConnection]
filteredConnections = [
  iperfConnection
    ]


connectionFilter = TestCase $ assertBool
  "Check connection is in the list"
  (iperfConnection `elem` filteredConnections)

-- connectionFilter = TestCase $ assertEqual
--   "Check connection is in the list"
--   True
--   ( iperfConnection `elem` filteredConnections)

-- main :: IO Count
main = do

  results <- runTestTT $ TestList [
      -- testEmpty
      -- , testCombo
      -- , testComboReverse,
      TestLabel "subflow is correctly filtered" connectionFilter
      , TestCase $ assertBool "connection should be equal" (iperfConnection == iperfConnection)
      ]
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
