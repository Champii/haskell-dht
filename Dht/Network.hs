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

  connect :: [String] -> IO (Maybe Handle)
  connect []            = return Nothing
  connect (host:port:_) = _connect host (PortNumber $ fromIntegral (read port :: Int))
  connect [host]        = _connect host (PortNumber 3000)

  _connect :: String -> PortID -> IO (Maybe Handle)
  _connect host port@(PortNumber iPort) = withSocketsDo $ do
    putStrLn $ "Connecting to bootstrap node: " ++ host ++ ":" ++ show iPort
    handle <- connectTo host port
    putStrLn "Connected"
    return (Just handle)
  _connect _ _ = return Nothing

  listen :: [Client] -> Hash -> Int -> IO ()
  listen clients hash port = withSocketsDo $ do
    sock <- listenOn $ PortNumber $ fromIntegral port
    putStrLn $ "Starting server on port " ++ show port
    handleConnections sock clients hash

  handleConnections :: Socket -> [Client] -> Hash -> IO ()
  handleConnections sock clients hash = do
    (handle, host, port) <- accept sock
    putStrLn $ "New client: " ++ host ++ show port
    maybHash <- firstPing handle hash

    let clients2 = maybe clients (addClient clients handle) maybHash
    let _ = fmap (forkRead handle) maybHash

    handleConnections sock clients2 hash

  addClient :: [Client] -> Handle -> Hash -> [Client]
  addClient clients handle hash = Client (length clients) handle hash : clients

  firstPing :: Handle -> Hash -> IO (Maybe Hash)
  firstPing handle hash = do
    ping handle hash
    mayb <- readOnce handle hash
    return $ fmap (\(Message clientHash _) -> clientHash) mayb

  forkRead :: Handle -> Hash -> IO ()
  forkRead handle hash = do
    _ <- forkIO $ readLoop handle hash
    return ()

  readLoop :: Handle -> Hash -> IO ()
  readLoop handle hash = do
    _ <- readOnce handle hash
    readLoop handle hash

  readOnce :: Handle -> Hash -> IO (Maybe Message)
  readOnce handle hash = do
    got <- B.hGetLine handle
    let eith = decode got :: Either String Message
    case eith of
      (Left err)    -> do
        putStrLn err
        return Nothing
      (Right msg) -> dispatch handle hash msg

  dispatch :: Handle -> Hash -> Message -> IO (Maybe Message)
  dispatch handle hash msg@(Message _ cmd) = do
    case cmd of
      Ping    -> do
        putStrLn "< Ping"
        pong handle hash
      Pong    -> putStrLn "< Pong"
      Unknown -> putStrLn "Unknown !"

    return (Just msg)
