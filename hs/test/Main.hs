module Main where

import System.Exit (exitFailure)
import Test.HUnit
import Generated
import IDiag

-- main = do
--     putStrLn "This test always fails!"
--     exitFailure

testEmpty = TestCase $ assertEqual
  "Check"
  1
  ( tcpStatesToWord [TcpEstablished] )

testCombo = TestCase $ assertEqual
  "Check"
  513
  ( tcpStatesToWord [TcpEstablished, TcpListen] )

testComboReverse = TestCase $ assertEqual
  "Check"
  513
  ( tcpStatesToWord [TcpEstablished, TcpListen] )

main = runTestTT $ TestList [testEmpty, testCombo, testComboReverse ]
