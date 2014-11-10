{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.Locutoria.Client where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (AddHandler, Handler)
import           Data.Default (Default, def)
import           Data.Map.Strict ((!))
import           Data.Maybe (catMaybes)
import           Reactive.Banana (Moment, compile, mapAccum, spill)
import           Reactive.Banana.Frameworks (Frameworks, actuate, fromAddHandler, reactimate)

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.Internal
import           Network.Mail.Locutoria.Notmuch

data ClientConfig = ClientConfig
  { clDb    :: Database
  , clQuery :: Query
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

data DataEvent = FetchChannels Query
               | FetchThreads Database [ChannelId]
               | FetchLikeCounts Database Index

data UiEvent = RenderChannels [ChannelId]
             | RenderThreads [ThreadInfo]
             | UiExit
  deriving Show

instance Default ClientState where
  def = ClientState
    { clIndex           = def
    , clSelectedChannel = Nothing
    }

locutoria :: ClientConfig
          -> ClientState
          -> AddHandler ClientEvent
          -> Handler UiEvent
          -> Handler DataEvent
          -> IO ()
locutoria config initState addEvent stepUi stepData' = do
  network <- compile $
    networkDescription config initState addEvent stepUi stepData'
  actuate network

networkDescription :: forall t. Frameworks t => ClientConfig
                                       -> ClientState
                                       -> AddHandler ClientEvent
                                       -> Handler UiEvent
                                       -> Handler DataEvent
                                       -> Moment t ()
networkDescription config initState addEvent stepUi stepData' = do
  clientEvents <- fromAddHandler addEvent
  let
    responseActions    = fmap (step config) clientEvents
    (responseLists, _) = mapAccum initState responseActions
    responses          = spill responseLists
  reactimate $ fmap (\e -> case e of
    DataResp e' -> stepData' e'
    UiResp e'   -> stepUi e'
    ) responses

step :: ClientConfig -> ClientEvent -> ClientState -> ([Response], ClientState)
step conf event s = case event of
  GetChannels ->
    ([DataResp (FetchChannels q)], s)

  GetThreads chan ->
    ([DataResp (FetchThreads db [chan])], s)

  GetAllThreads ->
    let cs = iChannels index
    in
    ([DataResp (FetchThreads db cs)], s)

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
    db    = clDb    conf
    q     = clQuery conf
    index = clIndex s

stepData :: Handler ClientEvent -> Handler DataEvent
stepData fire e = case e of
  FetchChannels q -> do
    index' <- fetchChannels q
    fire (IndexUpdate index')

  FetchThreads db chans -> do
    index' <- fetchThreads db chans
    fire (IndexUpdate index')

  FetchLikeCounts db index -> do
    index' <- fetchLikeCounts db index
    fire (IndexUpdate index')

updateUi :: ClientState -> [Response]
updateUi s = map UiResp $ catMaybes
  [ Just $ RenderChannels (iChannels idx)
  , (\c -> RenderThreads  (iThreads idx ! c)) <$> chan
  ]
  where
    idx  = clIndex s
    chan = clSelectedChannel s
