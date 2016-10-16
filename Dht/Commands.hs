module Dht.Commands
( ping
, pong
) where

  import System.IO
  import Data.Serialize
  import qualified Data.ByteString.Char8 as B

  import Dht.Data

  ping :: Handle -> Hash -> IO ()
  ping handle hash = do
    putStrLn "> Ping"
    send (Message hash Ping) handle

  pong :: Handle -> Hash -> IO ()
  pong handle hash = do
    putStrLn "> Pong"
    send (Message hash Pong) handle

  send :: Message -> Handle -> IO ()
  send msg handle = B.hPutStrLn handle $ encode msg
