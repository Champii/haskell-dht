module Dht.Network
( connect
, listen
, firstPing
, readLoop
) where

  import Network
  import System.IO
  import Data.Serialize
  import Data.Maybe
  import Control.Concurrent (forkIO)
  import qualified Data.ByteString.Char8 as B

  import Dht.Data
  import Dht.Commands

  connect :: [String] -> IO Handle
  connect (host:port:_) = _connect host (PortNumber $ fromIntegral (read port :: Int))
  connect [host]        = _connect host (PortNumber 3000)

  _connect :: String -> PortID -> IO Handle
  _connect host port@(PortNumber iPort) = withSocketsDo $ do
    putStrLn $ "Connecting to bootstrap node: " ++ host ++ ":" ++ show iPort
    handle <- connectTo host port
    putStrLn "Connected"
    return handle

  listen :: [Client] -> Hash -> Int -> IO ()
  listen clients hash port = withSocketsDo $ do
    sock <- listenOn $ PortNumber $ fromIntegral port
    putStrLn $ "Starting server on port " ++ show port
    handleConnections sock clients hash

  handleConnections :: Socket -> [Client] -> Hash -> IO ()
  handleConnections sock clients hash = do
    (handle, host, port) <- accept sock
    putStrLn $ "New client: " ++ host ++ show port
    cHash <- firstPing hash handle

    let clients2 = addClient clients handle cHash
    let _ = forkRead handle cHash

    handleConnections sock clients2 hash

  addClient :: [Client] -> Handle -> Hash -> [Client]
  addClient clients handle hash = Client (length clients) handle hash : clients

  firstPing :: Hash -> Handle -> IO Hash
  firstPing hash handle = do
    ping hash handle
    Message cHash _ <- readOnceDispatch handle hash
    return cHash

  forkRead :: Handle -> Hash -> IO ()
  forkRead handle hash = do
    forkIO $ readLoop handle hash
    return ()

  readLoop :: Handle -> Hash -> IO ()
  readLoop handle hash = do
    readOnceDispatch handle hash
    readLoop handle hash

  readOnce :: Handle -> Hash -> IO (Maybe Message)
  readOnce handle hash = do
    got <- B.hGetLine handle
    let eith = decode got :: Either String Message
    case eith of
      (Left err)    -> do
        putStrLn err
        return Nothing
      (Right msg) -> return $ Just msg

  dispatch :: Handle -> Hash -> Maybe Message -> IO (Maybe Message)
  dispatch handle hash Nothing = return Nothing
  dispatch handle hash maybMsg@(Just msg@(Message _ cmd)) = do
    case cmd of
      Ping    -> do
        putStrLn "< Ping"
        pong hash handle
      Pong    -> putStrLn "< Pong"
      Unknown -> putStrLn "Unknown !"

    return maybMsg

  readOnceDispatch :: Handle -> Hash -> IO Message
  readOnceDispatch handle hash =
    readOnce handle hash >>=
    dispatch handle hash >>=
    \(Just msg) -> return msg
