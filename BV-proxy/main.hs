import qualified Network as N
import qualified Network.BSD as N
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString.Lazy as NSBL
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as LBS

import System.Posix.Graceful

main :: IO ()
main = graceful settings worker
    where
      settings = GracefulSettings
                 { gracefulSettingsListen = N.listenOn $ N.PortNumber 8080
                 , gracefulSettingsWorkerCount = 1
                 , gracefulSettingsSockFile = "/tmp/proxy.sock"
                 , gracefulSettingsPidFile = "/tmp/proxy.pid"
                 , gracefulSettingsBinary = "/tmp/proxy"
                 }
      worker = GracefulWorker { gracefulWorkerInitialize = newTVarIO (5 :: Int)
                              , gracefulWorkerApplication = application
                              , gracefulWorkerFinalize = const $ return ()
                              }
      application sock limit = do
        n <- atomically $ do
               n <- readTVar limit
               if 0 < n
                 then writeTVar limit (n-1) >> return n
                 else retry
        putStrLn $ "LIMIT: " ++ show n ++ " -> " ++ show (n-1)
        bracket (NS.socket NS.AF_INET NS.Stream 0) NS.close $ \s -> do
          entry <- N.getHostByName "icfpc2013.cloudapp.net"
          NS.connect s $ NS.SockAddrInet 80 $ head $ N.hostAddresses entry
          forkIO $ proxyAll sock s
          proxyAll s sock
          return ()
        forkIO $ do
          threadDelay (25 * 1000 * 1000)
          n <- atomically $ do
                 n <- readTVar limit
                 writeTVar limit (n+1)
                 return n
          putStrLn $ "LIMIT: " ++ show n ++ " -> " ++ show (n+1)
        return ()
      proxyAll from to = do
        req <- NSBL.recv from 1024
        -- LBS.putStr req
        if LBS.null req
           then return ()
           else NSBL.send to req >> proxyAll from to
