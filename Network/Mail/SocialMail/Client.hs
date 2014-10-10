module Network.Mail.SocialMail.Client where

import Control.Monad (join, mapM)
import Data.Text (Text, unpack)
import Reactive.Banana (accumB)

import Network.Mail.SocialMail.Internal
import Network.Mail.SocialMail.Notmuch

data ClientConfig = ClientConfig
  { clDb :: Database
  }

data ClientState = ClientState
  { clChannels :: [ChannelId]
  }

data ClientEvent = GetChannels
                 | GotChannels [ChannelId]
                 | GetThreads ChannelId
                 | GetAllThreads 
                 | GotThreads [ThreadInfo]
                 | ClientExit
  deriving Show

data Response = DataResp DataEvent | UiResp UiEvent
  deriving Show

data DataEvent = FetchChannels Database
               | FetchThreads Database [ChannelId]
  deriving Show

data UiEvent = RenderChannels [ChannelId]
             | RenderThreads [ThreadInfo]
             | UiExit
             | UiNoop
  deriving Show

step :: ClientConfig -> ClientEvent -> ClientState -> (Response, ClientState)
step conf GetChannels s =
  let db = clDb conf
  in
  (DataResp (FetchChannels db), s)

step conf (GotChannels cs) s =
  let s' = s { clChannels = cs }
  in
  (UiResp (RenderChannels cs), s')

step conf (GetThreads chan) s =
  let db = clDb conf
  in
  (DataResp (FetchThreads db [chan]), s)

step conf GetAllThreads s =
  let db = clDb conf
      cs = clChannels s
  in
  (DataResp (FetchThreads db cs), s)

step conf (GotThreads ts) s =
  (UiResp (RenderThreads ts), s)

step _ ClientExit s = (UiResp UiExit, s)

stepData :: (ClientEvent -> IO ()) -> DataEvent -> IO ()
stepData fire e = case e of
  FetchChannels db -> do
    addrs <- getListAddrs db
    fire (GotChannels addrs)

  FetchThreads db chans -> do
    ts <- mapM (getThreads db) chans
    fire (GotThreads (join ts))

initState :: ClientState
initState = ClientState
  { clChannels = []
  }
