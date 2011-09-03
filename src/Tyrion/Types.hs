{-# LANGUAGE OverloadedStrings #-}

module Tyrion.Types where

import Data.Aeson
import Control.Applicative
import Control.Monad (MonadPlus(..))

data Repository = Repository { repoName :: String,
                               repoUrl :: String } deriving (Show, Eq)

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "name" <*>
                           v .: "url"
    parseJSON _          = mzero

data Push = Push { pushRepo :: Repository,
                   pushRef :: String } deriving (Show, Eq)

instance FromJSON Push where
    parseJSON (Object v) = Push <$>
                           v .: "repository" <*>
                           v .: "ref_name"
    parseJSON _          = mzero

data Payload = Payload { payloadPush :: Push } deriving (Show, Eq)

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$>
                           v .: "payload"
    parseJSON _          = mzero


