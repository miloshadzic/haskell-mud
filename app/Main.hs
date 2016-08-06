module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

-- Socket handling code from real world haskell
type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String
         -> HandlerFunc
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
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
        handle lock clientaddr
          "syslogtcpserver.hs: client connnected"
        forkIO $ procMessages lock connsock clientaddr
        procRequests lock mastersock
    procMessages :: MVar () -> Socket -> SockAddr -> IO ()
    procMessages lock connsock clientaddr =
      do
        connhdl <- socketToHandle connsock ReadMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        mapM_ (handle lock clientaddr) (lines messages)
        hClose connhdl
        handle lock clientaddr
          "syslogtcpserver.hs: client disconnected"
    handle :: MVar () -> HandlerFunc
    handle lock clientaddr msg =
      withMVar lock
        (\a -> handlerfunc clientaddr msg >> return a)


echoHandler :: HandlerFunc
echoHandler _ = putStrLn

main = serveLog "5555" echoHandler
