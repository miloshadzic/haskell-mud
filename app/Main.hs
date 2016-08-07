module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

-- Socket handling partly from real world haskell
type HandlerFunc = SockAddr -> String -> IO ()

serveEcho :: String
          -> HandlerFunc
          -> IO ()

serveEcho port handlerfunc = withSocketsDo $
  do addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just port)
     let serveraddr = head addrinfos

     sock <- socket (addrFamily serveraddr) Stream defaultProtocol

     bindSocket sock (addrAddress serveraddr)

     listen sock 5

     lock <- newMVar ()
     procRequests lock sock

  where
    procRequests :: MVar () -> Socket -> IO ()
    procRequests lock mastersock =
      do
        (connsock, clientaddr) <- accept mastersock
        forkIO $ procMessages lock connsock clientaddr
        procRequests lock mastersock
    procMessages :: MVar () -> Socket -> SockAddr -> IO ()
    procMessages lock connsock clientaddr =
      do
        connhdl <- socketToHandle connsock ReadWriteMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        withMVar lock
          (\a -> mapM_ (hPutStrLn connhdl) (lines messages) >> return a)
        hClose connhdl


echoHandler :: HandlerFunc
echoHandler _ = putStrLn

main = serveEcho "5555" echoHandler
