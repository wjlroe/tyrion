{-# LANGUAGE OverloadedStrings #-}

module Tyrion.Types where

import Data.Aeson
import Control.Applicative
import Control.Monad (MonadPlus(..))

data Person = Person {
      personName :: String,
      personEmail :: String
    } deriving (Show, Eq)

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "email"
    parseJSON _          = mzero

data PersonWithUsername = PersonWithUsername {
      personUName :: String,
      personUUsername :: String,
      personUEmail :: String
    } deriving (Show, Eq)

instance FromJSON PersonWithUsername where
    parseJSON (Object v) = PersonWithUsername <$>
                           v .: "name" <*>
                           v .: "username" <*>
                           v .: "email"
    parseJSON _          = mzero

data Repository = Repository {
      repoName :: String,
      repoSize :: Integer,
      repoHasWiki :: Bool,
      repoCreatedAt :: String,
      repoPrivate :: Bool,
      repoWatchers :: Integer,
      repoUrl :: String,
      repoFork :: Bool,
      --repoLanguage :: String,
      repoPushedAt :: String,
      repoOpenIssues :: Integer,
      repoHasDownloads :: Bool,
      repoHasIssues :: Bool,
      repoHomepage :: String,
      repoForks :: Integer,
      repoDescription :: String,
      repoOwner :: Person
    } deriving (Show, Eq)

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "name" <*>
                           v .: "size" <*>
                           v .: "has_wiki" <*>
                           v .: "created_at" <*>
                           v .: "private" <*>
                           v .: "watchers" <*>
                           v .: "url" <*>
                           v .: "fork" <*>
                           --v .: "language" <*>
                           v .: "pushed_at" <*>
                           v .: "open_issues" <*>
                           v .: "has_downloads" <*>
                           v .: "has_issues" <*>
                           v .: "homepage" <*>
                           v .: "forks" <*>
                           v .: "description" <*>
                           v .: "owner"
    parseJSON _          = mzero

data Files = Files {
      filesModified :: [String],
      filesAdded :: [String],
      filesRemoved :: [String]
    } deriving (Show, Eq)

instance FromJSON Files where
    parseJSON (Object v) = Files <$>
                           v .: "modified" <*>
                           v .: "added" <*>
                           v .: "removed"
    parseJSON _          = mzero

data Commit = Commit {
      commitTimestamp :: String,
      commitAuthor :: PersonWithUsername,
      commitFiles :: Files,
      commitUrl :: String,
      commitId :: String,
      commitDistinct :: Bool,
      commitMessage :: String
    } deriving (Show, Eq)

instance FromJSON Commit where
    parseJSON (Object v) = Commit <$>
                           v .: "timestamp" <*>
                           v .: "author" <*>
                           v .: "files" <*>
                           v .: "url" <*>
                           v .: "id" <*>
                           v .: "distinct" <*>
                           v .: "message"
    parseJSON _          = mzero

data Push = Push {
      pushPusher :: Person,
      pushRepo :: Repository,
      pushRefName :: String,
      pushForced :: Bool,
      pushAfter :: String,
      pushDeleted :: Bool,
      pushRef :: String,
      pushCommits :: [Commit],
      pushBefore :: String,
      pushBaseRef :: String,
      pushCompare :: String,
      pushCreated :: Bool
    } deriving (Show, Eq)

instance FromJSON Push where
    parseJSON (Object v) = Push <$>
                           v .: "pusher" <*>
                           v .: "repository" <*>
                           v .: "ref_name" <*>
                           v .: "forced" <*>
                           v .: "after" <*>
                           v .: "deleted" <*>
                           v .: "ref" <*>
                           v .: "commits" <*>
                           v .: "before" <*>
                           v .: "base_ref" <*>
                           v .: "compare" <*>
                           v .: "created"
    parseJSON _          = mzero

data Payload = Payload {
      payloadPush :: Push
    } deriving (Show, Eq)

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$>
                           v .: "payload"
    parseJSON _          = mzero


