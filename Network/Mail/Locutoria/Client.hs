{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.Locutoria.Client where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (AddHandler, Handler)
import           Control.Monad (mplus)
import           Data.Default (Default, def)
import qualified Data.Map.Strict as Map
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
  , clRefreshing      :: Bool
  }

data ClientEvent = Refresh
                 | SetChannel (Maybe ChannelId)
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
    , clRefreshing      = False
    }

locutoria :: ClientConfig
          -> AddHandler ClientEvent
          -> Handler UiEvent
          -> Handler DataEvent
          -> IO ()
locutoria config addEvent stepUi stepData' = do
  network <- compile $
    networkDescription config addEvent stepUi stepData'
  actuate network

networkDescription :: forall t. Frameworks t => ClientConfig
                                       -> AddHandler ClientEvent
                                       -> Handler UiEvent
                                       -> Handler DataEvent
                                       -> Moment t ()
networkDescription config addEvent stepUi stepData' = do
  clientEvents <- fromAddHandler addEvent
  let
    initState          = def
    responseActions    = fmap (step config) clientEvents
    (responseLists, _) = mapAccum initState responseActions
    responses          = spill responseLists
  reactimate $ fmap (\e -> case e of
    DataResp e' -> stepData' e'
    UiResp e'   -> stepUi e'
    ) responses

step :: ClientConfig -> ClientEvent -> ClientState -> ([Response], ClientState)
step conf event s = case event of
  Refresh ->
    let refreshing = clRefreshing s
        cs = iChannels index
        s' = s { clRefreshing = True }
    in
    if not refreshing
    then (map DataResp
      [ FetchChannels q
      , FetchThreads db cs
      , FetchLikeCounts db index
      ], s')
    else ([], s)

  SetChannel chan ->
    let s' = s { clSelectedChannel = chan }
    in
    (updateUi s s', s')

  IndexUpdate f ->
    let index' = f index
        s' = s { clIndex = index', clRefreshing = False }
    in
    (updateUi s s', s')

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

updateUi :: ClientState -> ClientState -> [Response]
updateUi prevState state = map UiResp $ catMaybes
  [ RenderChannels <$> iChanged iChannels
  , RenderThreads  <$> threads
  ]
  where
    iChanged f = if (f prevIdx /= f idx) then Just (f idx) else Nothing
    sChanged f = if (f prevState /= f state) then Just (f state) else Nothing
    idx        = clIndex state
    prevIdx    = clIndex prevState
    threadsChanged = (const True <$> iChanged iThreads) `mplus` (const True <$> sChanged clSelectedChannel)
    threads    = do
      _       <- threadsChanged
      curChan <- clSelectedChannel state
      Map.lookup curChan (iThreads idx)

instance Show ClientEvent where
  show Refresh           = "Refresh"
  show (SetChannel chan) = "SetChannel " ++ show chan
  show (IndexUpdate _)   = "IndexUpdate"
  show ClientExit        = "ClientExit"
