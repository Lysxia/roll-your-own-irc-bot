{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- First step: connecting to a server -}

import System.IO                      -- base
import qualified Network.Socket as N  -- network

-- Configuration options
myServer = "irc.freenode.org" :: String
myPort   = 6667 :: N.PortNumber

-- Toplevel program
main :: IO ()
main = do
    h <- connectTo myServer myPort
    hSetBuffering stdout NoBuffering
    t <- hGetContents h
    print t

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
