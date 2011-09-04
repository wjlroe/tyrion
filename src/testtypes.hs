import Tyrion.Types
import Data.Aeson
import Control.Monad.Error
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as B

main = do
  jsonData <- readFile "/Users/will/Desktop/push.json"
  let result = case A.parse Aeson.json (B.pack jsonData) of
                 A.Done _ a     -> fromJSON a
                 A.Fail _ _ err -> Error err
  case result of
    Success payload ->
        do
          putStrLn $ "push: " ++ (show (payload :: Payload))

    Error err ->
        do
          putStrLn $ "Error: " ++ err
