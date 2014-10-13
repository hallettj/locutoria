{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Client where

import Control.Monad (join, mapM)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
import Reactive.Banana (accumB)

import           Network.Mail.SocialMail.Index
import qualified Network.Mail.SocialMail.Index as Index
import           Network.Mail.SocialMail.Internal
import           Network.Mail.SocialMail.Notmuch

data ClientConfig = ClientConfig
  { clDb :: Database
  }

data ClientState = ClientState
  { clIndex           :: Index
  , clSelectedChannel :: Maybe ChannelId
  }

data ClientEvent = GetChannels
                 | GetThreads ChannelId
                 | GetAllThreads
                 | GetLikeCounts
                 | IndexUpdate (Index -> Index)
                 | ClientExit

data Response = DataResp DataEvent | UiResp UiEvent | Noop

data DataEvent = FetchChannels Database Index
               | FetchThreads Database Index [ChannelId]
               | FetchLikeCounts Database Index

data UiEvent = RenderChannels [ChannelId]
             | RenderThreads [ThreadInfo]
             | UiExit
  deriving Show

step :: ClientConfig -> ClientEvent -> ClientState -> (Response, ClientState)
step conf event s = case event of
  GetChannels ->
    (DataResp (FetchChannels db index), s)

  GetThreads chan ->
    (DataResp (FetchThreads db index [chan]), s)

  GetAllThreads ->
    let cs = iChannels index
    in
    (DataResp (FetchThreads db index cs), s)

  GetLikeCounts ->
    (DataResp (FetchLikeCounts db index), s)

  IndexUpdate f ->
    let index' = f index
        s' = s { clIndex = index' }
    in
    (UiResp undefined, s')  -- TODO: update UI

  ClientExit ->
    (UiResp UiExit, s)

  where
    db    = clDb conf
    index = clIndex s

stepData :: (ClientEvent -> IO ()) -> DataEvent -> IO ()
stepData fire e = case e of
  FetchChannels db index -> do
    index' <- fetchChannels db index
    fire (IndexUpdate index')

  FetchThreads db index chans -> do
    index' <- fetchThreads db index chans
    fire (IndexUpdate index')

  FetchLikeCounts db index -> do
    index' <- fetchLikeCounts db index
    fire (IndexUpdate index')

initState :: ClientState
initState = ClientState
  { clIndex           = Index.empty
  , clSelectedChannel = Nothing
  }
