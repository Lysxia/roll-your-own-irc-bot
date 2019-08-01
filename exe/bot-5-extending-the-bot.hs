{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Control.Exception              -- base
import Control.Monad.IO.Class         --
import Data.List                      --
import System.Exit                    --
import System.IO                      --
import qualified Network.Socket as N  -- network
import Control.Monad.Trans.Reader     -- transformers
import Data.Time                      -- time

myServer = "irc.freenode.org" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "#tutbot-testing" :: String
myNick   = "tutbot" :: String

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop st = runReaderT run st

--

data Bot = Bot { botSocket :: Handle, startTime :: UTCTime }

type Net = ReaderT Bot IO

connect :: IO Bot
connect = notify $ do
    t <- getCurrentTime
    h <- connectTo myServer myPort
    return (Bot h t)
  where
    notify a = bracket_
      (putStrLn ("Connecting to " ++ myServer ++ " ...") >> hFlush stdout)
      (putStrLn "done.")
      a

connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

--

run :: Net ()
run = do
    write "NICK" myNick
    write "USER" (myNick ++ " 0 * :tutorial bot")
    write "JOIN" myChan
    listen

write :: String -> String -> Net ()
write cmd args = do
    h <- asks botSocket
    let msg = cmd ++ " " ++ args ++ "\r\n"
    liftIO $ hPutStr h msg          -- Send message on the wire
    liftIO $ putStr ("> " ++ msg)   -- Show sent message on the command line

listen :: Net ()
listen = forever $ do
    h <- asks botSocket
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    if isPing s then pong s else eval (clean s)
  where
    forever :: Net () -> Net ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval "!uptime" = uptime >>= privmsg
eval "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval x | "!id" `isPrefixOf` x = privmsg (drop 4 x)
eval _ = return ()  -- ignore everything else

privmsg :: String -> Net ()
privmsg msg = write "PRIVMSG" (myChan ++ " :" ++ msg)

--

uptime :: Net String
uptime = do
  now <- liftIO getCurrentTime
  zero <- asks startTime
  return (pretty (diffUTCTime now zero))

pretty :: NominalDiffTime -> String
pretty diff =
    unwords
      . map (\(t, unit) -> show t ++ unit)
      $ if null diffs then [(0, "s")] else diffs
  where
    diffs :: [(Integer, String)]
    diffs = filter ((/= 0) . fst)
      $ decompose [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")] (floor diff)
    decompose [] _ = []
    decompose ((secs, unit) : metrics) t =
      let (n, t') = t `divMod` secs
      in (n, unit) : decompose metrics t'