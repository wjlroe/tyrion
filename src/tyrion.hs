import Network.AMQP
import Data.ConfigFile
import Control.Monad.Error
import Data.Either.Utils
import System.Exit (exitSuccess)
import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as B
import System(getArgs)
import System.IO hiding (readFile)
import System.Log.Logger
import System.Log.Handler.Syslog
import Tyrion.Types
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Attoparsec.Lazy as A
-- import Data.Attoparsec

exitHandler :: IO ()
exitHandler = exitSuccess

githubQueue = "github"
githubRouting = "github.push.*.*.*"

githubConfig :: String -> IO (String, String, String, String)
githubConfig fileLoc = do
  config <- readfile emptyCP fileLoc
  let cp = forceEither config
      username = forceEither $ get cp "" "username"
      password = forceEither $ get cp "" "password"
      exchange = forceEither $ get cp "" "exchange"
      vhost    = forceEither $ get cp "" "vhost"
  return $ (username, password, exchange, vhost)

processPush :: Payload -> IO()
processPush p = do
  putStrLn $ "push! " ++ (show p)

githubPush :: (Message, Envelope) -> IO ()
githubPush (msg,env) = do
  putStrLn $ "Got: " ++ (C.unpack body)
  let result = case A.parse Aeson.json body of
                 A.Done _ a     -> fromJSON a
                 A.Fail _ _ err -> Error err
  case result of
    Success payload ->
        do

          processPush (payload :: Payload)
          ackEnv env
    Error err ->
        do
          errorM "json" ("JSON Error: " ++ err)
          ackEnv env
  where body = msgBody msg

main = do
  s <- openlog "tyrion" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  args <- getArgs
  let fileLoc = head(args)
  (username, password, exchangeName, vhost) <- githubConfig fileLoc
  conn <- openConnection "127.0.0.1" vhost username password
  chan <- openChannel conn
  exchange <- declareExchange chan newExchange {exchangeName = exchangeName, exchangeType = "topic"}
  declareQueue chan newQueue {queueName = githubQueue}
  bindQueue chan githubQueue exchangeName githubRouting
  consumeMsgs chan githubQueue Ack githubPush
  installHandler sigHUP (Catch exitHandler) (Just fullSignalSet)
  forever $ do threadDelay (10^6)
  closeConnection conn
