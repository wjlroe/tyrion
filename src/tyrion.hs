import Network.AMQP
import Data.ConfigFile
import Control.Monad.Error
import Data.Either.Utils
import System.Exit (exitSuccess)
import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Monad (forever, ap, liftM)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as B
import System(getArgs)
import System.IO hiding (readFile)
import System.Log.Logger
import System.Log.Handler.Syslog
import Tyrion.Types
import Data.Aeson
import Directory
import System.FilePath
import System.Cmd (system)
import System.Exit (ExitCode(..))
import Data.String.Utils (replace)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Attoparsec.Lazy as A
-- import Data.Attoparsec

exitHandler :: IO ()
exitHandler = exitSuccess

githubQueue = "github"
githubRouting = "github.push.*.*.*"

githubConfig :: String -> IO (String, String, String, String, FilePath)
githubConfig fileLoc = do
  config <- readfile emptyCP fileLoc
  let cp = forceEither config
      username = forceEither $ get cp "" "username"
      password = forceEither $ get cp "" "password"
      exchange = forceEither $ get cp "" "exchange"
      vhost    = forceEither $ get cp "" "vhost"
      rootdir  = forceEither $ get cp "" "rootdir"
  return $ (username, password, exchange, vhost, rootdir)

mkdirP :: FilePath -> IO ()
mkdirP filepath = do
  fexist <- doesDirectoryExist filepath
  case fexist of
    True -> do
        return $ ()
    False -> do
        let parent = takeDirectory filepath
        pexist <- doesDirectoryExist parent
        case pexist of
          True -> do
              createDirectory filepath
          False -> do
              mkdirP parent
              createDirectory filepath

initialClone :: FilePath -> String -> IO ()
initialClone clone_dir giturl = do
  cloneExists <- doesDirectoryExist clone_dir
  case cloneExists of
    True -> do
      putStrLn "Already cloned"
      return $ ()
    False -> do
      mkdirP clone_dir
      let clone_cmd = "git clone " ++ giturl ++ " " ++ clone_dir
      wrap clone_cmd

createReleasesDir :: FilePath -> IO ()
createReleasesDir releaseDir = do
  releaseExists <- doesDirectoryExist releaseDir
  case releaseExists of
    True -> do
      putStrLn "releases/ already exists"
      return $ ()
    False -> do
      mkdirP releaseDir

wrap :: String -> IO ()
wrap action = do
  putStrLn $ "Running: " ++ action
  code <- system(action)
  case code of
    ExitSuccess -> return ()
    ExitFailure err -> fail $ "Error running command: " ++ action ++ " Exit status: " ++ show err

processPush :: FilePath -> Payload -> IO ()
processPush rootdir p = do
  putStrLn $ "push! " ++ (show p)
  let payload       = payloadPush p
      repo          = pushRepo payload
      latestCommit  = last . pushCommits $ payload
      sha           = commitId latestCommit
      url           = repoUrl repo
      giturl        = "git@" ++ (replace "github.com/" "github.com:" (replace "https://" "" url)) ++ ".git"
      name          = repoName repo
      releaseDir    = joinPath [rootdir, name, "releases"]
      shaDir        = joinPath [releaseDir, sha]
      currentDir    = joinPath [rootdir, name, "current"]
      cloneDir      = joinPath [rootdir, name, "shared", "cached-copy"]
      syncCmd       = "cd " ++ cloneDir ++ " && git fetch && git reset --hard " ++ sha
      releaseCmd    = "rm -fr " ++ shaDir ++ " ; cp -r " ++ cloneDir ++ " " ++ shaDir
      linkCmd       = "rm " ++ currentDir ++ " ; ln -s " ++ shaDir ++ " " ++ currentDir
      compileCmd    = "cd " ++ currentDir ++ " && make"
      upstartCmd    = "cd " ++ currentDir ++ " && sudo foreman export upstart /etc/init -u deploy -a deploy-" ++ name
      stopCmd       = "sudo stop deploy-" ++ name ++ " ; echo 'fine'; sleep 1"
      startCmd      = "sudo start deploy-" ++ name
  putStrLn $ "dir: " ++ shaDir
  putStrLn $ "current: " ++ currentDir
  putStrLn $ "shared: " ++ cloneDir
  -- each function is of type something -> IO(ExitCode)
  initialClone cloneDir giturl
  createReleasesDir releaseDir
  wrap syncCmd
  wrap releaseCmd
  wrap linkCmd
  wrap compileCmd
  wrap upstartCmd
  wrap stopCmd
  wrap startCmd
  -- /home/deploy/projects/name/releases/49875345/
  -- create directories
  -- releases
  -- checkout source
  -- link current -> release

githubPush :: FilePath -> (Message, Envelope) -> IO ()
githubPush rootdir (msg,env) = do
  putStrLn $ "Got: " ++ (C.unpack body)
  let result = case A.parse Aeson.json body of
                 A.Done _ a     -> fromJSON a
                 A.Fail _ _ err -> Error err
  case result of
    Success payload ->
        do

          processPush rootdir (payload :: Payload)
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
  (username, password, exchangeName, vhost, rootdir) <- githubConfig fileLoc
  conn <- openConnection "127.0.0.1" vhost username password
  chan <- openChannel conn
  exchange <- declareExchange chan newExchange {exchangeName = exchangeName, exchangeType = "topic"}
  declareQueue chan newQueue {queueName = githubQueue}
  bindQueue chan githubQueue exchangeName githubRouting
  consumeMsgs chan githubQueue Ack (githubPush rootdir)
  installHandler sigHUP (Catch exitHandler) (Just fullSignalSet)
  forever $ do threadDelay (10^6)
  closeConnection conn


somejson :: IO (String)
somejson = do
  contents <- readFile "/Users/will/Desktop/push2.json"
  return $ contents

somepush :: IO ()
somepush = do
  something <- somejson
  let result = case A.parse Aeson.json (C.pack something) of
                 A.Done _ a     -> fromJSON a
                 A.Fail _ _ err -> Error err
  case result of
    Success payload ->
        do
          processPush "/home/deploy" (payload :: Payload)
    Error err ->
        do
          putStrLn $ "error: " ++ err

