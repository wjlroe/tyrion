import Network.AMQP
import Data.ConfigFile
import Control.Monad.Error
import Data.Either.Utils
import System.Exit (exitSuccess)
import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as C
import System(getArgs)

exitHandler :: IO ()
exitHandler = exitSuccess

githubQueue = "github"
githubExchange = ""
githubRouting = "github.push.*.*.*"

githubConfig :: String -> IO (String, String)
githubConfig fileLoc = do
  config <- readfile emptyCP fileLoc
  let cp = forceEither config
      username = forceEither $ get cp "" "username"
      password = forceEither $ get cp "" "password"
  return $ (username, password)

githubPush :: (Message, Envelope) -> IO ()
githubPush (msg,env) = do
  putStrLn $ "Got: " ++ decodedMsg
  ackEnv env
  where decodedMsg = (C.unpack $ msgBody msg)

main = do
  args <- getArgs
  let fileLoc = head(args)
  (username, password) <- githubConfig fileLoc
  conn <- openConnection "127.0.0.1" "/github" username password
  chan <- openChannel conn
  exchange <- declareExchange chan newExchange {exchangeName = githubExchange, exchangeType = "topic"}
  bindQueue chan githubQueue githubExchange githubRouting
  consumeMsgs chan githubQueue Ack githubPush
  installHandler sigHUP (Catch exitHandler) (Just fullSignalSet)
  forever $ do threadDelay (10^6)
  closeConnection conn
