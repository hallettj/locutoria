module Network.Mail.SocialMail.Client where

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

data ClientEvent = GetChannels (Widget Channels)
                 | GetThreads (Widget Threads) ChannelId
                 | GetAllThreads (Widget Threads)
                 | Exit
  deriving Show

step :: ClientConfig -> ClientEvent -> IO ()

step conf (GetChannels w) = do
  addrs <- getListAddrs
  renderChannels w addrs

step conf (GetThreads w chan) = do
  let db = clDb conf
  ts <- getThreads db chan
  renderThreads w ts

step _ Exit = exitSuccess

state = accumB initState

initState :: ClientState
initState = ClientState
  { clChannels = []
  }
