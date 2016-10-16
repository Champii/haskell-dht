module Dht.Data
( Command(Unknown, Ping, Pong)
, Message(Message)
, Client(Client)
, Hash
) where

  import System.IO
  import Data.Serialize
  import qualified Data.ByteString.Char8 as B

  type Hash = B.ByteString

  data Command = Unknown | Ping | Pong deriving (Show, Read)

  data Message = Message Hash Command deriving (Show, Read)

  data Client = Client Int Handle Hash

  instance Serialize Command where
    put c = case c of
      Unknown -> put (0 :: Int)
      Ping    -> put (1 :: Int)
      Pong    -> put (2 :: Int)

    get = do
      t <- get :: Get Int
      case t of
        0 -> return Unknown
        1 -> return Ping
        2 -> return Pong

  instance Serialize Message where
    put (Message h c) = do
      put h
      put c

    get = do
      h <- get :: Get Hash
      c <- get :: Get Command
      return $ Message h c
