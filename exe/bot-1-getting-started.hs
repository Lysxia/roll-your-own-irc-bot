{-# OPTIONS_GHC -Wno-missing-signatures #-}

import System.IO                      -- base
import qualified Network.Socket as N  -- network

myServer = "irc.freenode.org" :: String
myPort   = 6667 :: N.PortNumber

main :: IO ()
main = do
    h <- connectTo myServer myPort
    hSetBuffering stdout NoBuffering
    t <- hGetContents h
    print t

--

connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
