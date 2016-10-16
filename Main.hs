import System.IO
import Data.Maybe
import System.Entropy
import System.Environment
import Data.Function.Memoize
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B

import Dht.Data
import Dht.Network

nop :: IO ()
nop = return ()

generateHash :: IO Hash
generateHash = memoize getEntropy 8

main :: IO ()
main = do
  hash <- generateHash

  (port:args) <- getArgs
  isConnected <- connect args

  case isConnected of
    Nothing       -> listen [] hash $ read port
    (Just handle) -> do
      maybHash <- firstPing handle hash
      let client = Client 1 handle $ fromJust maybHash
        in listen [client] hash $ read port
