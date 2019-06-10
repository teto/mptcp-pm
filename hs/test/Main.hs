module Main where

import System.Exit (exitFailure)
import Test.HUnit
import Generated
import IDiag
import Net.Mptcp

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



filteredConnections :: [TcpConnection]
filteredConnections = [
    TcpConnection {
        srcIp = fromIPv4 localhost
        , dstIp = fromIPv4 localhost
        , srcPort = iperfClientPort
        , dstPort = iperfServerPort
        -- placeholder values
        , priority = Nothing
        , subflowInterface = Nothing
        , localId = 0
        , remoteId = 0
        , inetFamily = eAF_INET
    }
    ]

main = runTestTT $ TestList [testEmpty, testCombo, testComboReverse ]
