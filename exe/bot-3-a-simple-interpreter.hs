{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- Third step: interpreting commands -}

import Data.List                      -- base
import System.Exit                    --
import System.IO                      --
import qualified Network.Socket as N  -- network

-- Configuration options
myServer = "irc.libera.chat" :: String
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
    let s = init line
    if isPing s then pong s else eval h (clean s)
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> IO ()
    pong x = write h "PONG" (':' : drop 6 x)

-- Dispatch a command
eval :: Handle -> String -> IO ()
eval h "!quit" = write h "QUIT" ":Exiting" >> exitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _ _ = return ()  -- ignore everything else

-- Send a privmsg to the channel
privmsg :: Handle -> String -> IO ()
privmsg h msg = write h "PRIVMSG" (myChan ++ " :" ++ msg)
