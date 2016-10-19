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

  clients <- connectIfRequested hash args
  listen clients hash $ read port
