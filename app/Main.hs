{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.Typeable
import Lib
import Network.Transport.TCP

{-

Code from the tutorial https://www.well-typed.com/blog/68/

-}

-- Serializable (= Binary + Typeable)
data Ping = Ping deriving (Typeable)

instance Binary Ping where
  put Ping = putWord8 0
  get = do getWord8; return Ping

data Pong = Pong deriving (Typeable)

instance Binary Pong where
  put Pong = putWord8 0
  get = do getWord8; return Pong

server :: ReceivePort (Ping, SendPort Pong) -> Process ()
server resp = do
  (Ping, port) <- receiveChan resp
  liftIO $ putStrLn "Got a ping!"

  sendChan port Pong

client :: SendPort (Ping, SendPort Pong) -> Process ()
client sPing = do
  (sPong, rPong) <- newChan
  sendChan sPing (Ping, sPong)

  Pong <- receiveChan rPong
  liftIO $ putStrLn "Got a pong!"

ignition :: Process ()
ignition = do
  -- start the server
  sPing <- spawnChannelLocal server
  -- start the client
  spawnLocal $ client sPing
  liftIO $ threadDelay 100000 -- wait a while

main :: IO ()
main = do
  Right transport <-
    createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node ignition
