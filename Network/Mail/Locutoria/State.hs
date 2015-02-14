{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.State where

import           Control.Lens ((^.), Getter, Lens, makeLenses, to)
import           Data.Default (Default, def)
import           Data.List (findIndex)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.Internal
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Notmuch

data State = State
  { _index           :: Index
  , _selectedChannel :: Maybe ChannelId
  , _selectedThread  :: Maybe ThreadId
  , _refreshing      :: Bool
  , _activity        :: Activity
  }
  deriving Show

data Activity = NoActivity
              | Compose ThreadId
              | Shutdown
  deriving Show


instance Default State where
  def = State { _index           = def
              , _selectedChannel = Nothing
              , _selectedThread  = Nothing
              , _refreshing      = False
              , _activity        = NoActivity
              }

makeLenses ''State

channels :: Lens State State [MailingList] [MailingList]
channels = index . lists

_conversations :: State -> [ThreadInfo]
_conversations s = fromMaybe [] ts
  where
    threadMap = _threads (_index s)
    ts = do
      channel <- _selectedChannel s
      Map.lookup channel threadMap

conversations :: Getter State [ThreadInfo]
conversations = to _conversations

currentChannelIndex :: State -> Maybe Int
currentChannelIndex s = findIndex (\ml -> s^.selectedChannel == Just (mlId ml)) (s^.channels)

currentConversationIndex :: State -> Maybe Int
currentConversationIndex s = findIndex (\c -> s^.selectedThread == Just (threadId c)) cs
  where
    cs = s^.conversations
    threadId (tId', _, _, _) = tId'
