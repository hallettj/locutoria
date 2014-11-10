{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.Locutoria.Client where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (AddHandler, Handler)
import           Control.Monad (join, mapM)
import           Data.List (foldl')
import           Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Text (Text, unpack)
import           Reactive.Banana (Moment, compile, filterE, mapAccum, spill)
import           Reactive.Banana.Frameworks (Frameworks, actuate, fromAddHandler, reactimate)

import           Network.Mail.Locutoria.Index
import qualified Network.Mail.Locutoria.Index as Index
import           Network.Mail.Locutoria.Internal
import           Network.Mail.Locutoria.Notmuch

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

data Response = DataResp DataEvent | UiResp UiEvent

data DataEvent = FetchChannels Database Index
               | FetchThreads Database Index [ChannelId]
               | FetchLikeCounts Database Index

data UiEvent = RenderChannels [ChannelId]
             | RenderThreads [ThreadInfo]
             | UiExit
  deriving Show

locutoria :: ClientConfig
          -> AddHandler ClientEvent
          -> Handler ClientEvent
          -> Handler UiEvent
          -> Handler DataEvent
          -> IO ()
locutoria config addEvent fireEvent stepUi stepData = do
  network <- compile $
    networkDescription config addEvent fireEvent stepUi stepData
  actuate network

networkDescription :: forall t. Frameworks t => ClientConfig
                                       -> AddHandler ClientEvent
                                       -> Handler ClientEvent
                                       -> Handler UiEvent
                                       -> Handler DataEvent
                                       -> Moment t ()
networkDescription config addEvent fire stepUi stepData = do
  clientEvents <- fromAddHandler addEvent
  let
    responseActions        = fmap (step config) clientEvents
    (responseLists, state) = mapAccum initState responseActions
    responses              = spill responseLists
  reactimate $ fmap (\e -> case e of
    DataResp e' -> stepData e'
    UiResp e'   -> stepUi e'
    ) responses

step :: ClientConfig -> ClientEvent -> ClientState -> ([Response], ClientState)
step conf event s = case event of
  GetChannels ->
    ([DataResp (FetchChannels db index)], s)

  GetThreads chan ->
    ([DataResp (FetchThreads db index [chan])], s)

  GetAllThreads ->
    let cs = iChannels index
    in
    ([DataResp (FetchThreads db index cs)], s)

  GetLikeCounts ->
    ([DataResp (FetchLikeCounts db index)], s)

  IndexUpdate f ->
    let index' = f index
        s' = s { clIndex = index' }
    in
    (updateUi s', s')

  ClientExit ->
    ([UiResp UiExit], s)

  where
    db    = clDb conf
    index = clIndex s

stepData :: Handler ClientEvent -> Handler DataEvent
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

updateUi :: ClientState -> [Response]
updateUi s = map UiResp $ catMaybes
  [ Just $ RenderChannels (iChannels idx)
  , (\c -> RenderThreads  (iThreads idx ! c)) <$> chan
  ]
  where
    idx  = clIndex s
    chan = clSelectedChannel s
