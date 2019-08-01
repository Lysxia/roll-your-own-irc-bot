{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- Second step: sending and receiving messages -}

import System.IO                      -- base
import qualified Network.Socket as N  -- network

-- Configuration options
myServer = "irc.freenode.org" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "#tutbot-testing" :: String
myNick   = "tutbot" :: String

-- Toplevel program
main :: IO ()
main = do
    h <- connectTo myServer myPort
    write h "NICK" myNick
    write h "USER" (myNick ++ " 0 * :tutorial bot")
    write h "JOIN" myChan
    listen h

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    let msg = cmd ++ " " ++ args ++ "\r\n"
    hPutStr h msg          -- Send message on the wire
    putStr ("> " ++ msg)   -- Show sent message on the command line

-- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
    line <- hGetLine h
    putStrLn line
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a
