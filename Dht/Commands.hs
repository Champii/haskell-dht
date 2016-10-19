module Dht.Commands
( ping
, pong
) where

  import System.IO
  import Data.Serialize
  import qualified Data.ByteString.Char8 as B

  import Dht.Data

  ping :: Hash -> Handle -> IO ()
  ping hash handle = do
    putStrLn "> Ping"
    send (Message hash Ping) handle

  pong :: Hash -> Handle -> IO ()
  pong hash handle = do
    putStrLn "> Pong"
    send (Message hash Pong) handle

  send :: Message -> Handle -> IO ()
  send msg handle = B.hPutStrLn handle $ encode msg
