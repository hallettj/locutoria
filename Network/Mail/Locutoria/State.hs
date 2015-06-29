{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.State where

import           Control.Applicative ((<$>))
import           Control.Lens hiding (Index)
import           Data.Default (def)
import           Data.List (elemIndex, find)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)

import           Network.Mail.Locutoria.Index hiding (_conversations, conversations)
import qualified Network.Mail.Locutoria.Index as Index
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.Notmuch (Database)
import           Network.Mail.Locutoria.Channel
import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.View

data State = State
  { _stDatabase   :: Database
  , _stIndex      :: Index
  , _stHistory    :: History
  , _stRefreshing :: Bool
  , _stScreenSize :: (Int, Int)
  , _stStatus     :: Status
  }
  deriving (Eq, Show)

type History = [View]

data Status = GenericError Text
            | Refreshing
            | Nominal
  deriving (Eq, Show)

makeLenses ''State

mkState :: Database -> State
mkState db = State
  { _stDatabase   = db
  , _stIndex      = def
  , _stHistory    = []
  , _stRefreshing = False
  , _stScreenSize = (80, 50)
  , _stStatus     = Nominal
  }

stView :: State -> View
stView state = case state^.stHistory of
  v:_ -> v
  []  -> Root

pushView :: View -> State -> State
pushView v = stHistory %~ (v:)

popView :: State -> State
popView = stHistory %~ drop 1

replaceView :: View -> State -> State
replaceView v = pushView v . popView

selectedChannel :: State -> Maybe Channel
selectedChannel = viewChannel . stView

selectedConversation :: State -> Maybe Conversation
selectedConversation = viewConversation . stView

selectedMessage :: State -> Maybe Message
selectedMessage = viewMessage . stView

channelGroups :: State -> [ChannelGroup]
channelGroups s =
  [ ChannelGroup "Direct"  [NoListChannel]
  , ChannelGroup "Flagged" [FlaggedChannel]
  , ChannelGroup "Lists"   ls
  ]
  where
    ls = ListChannel <$> lists (s^.stIndex)

conversations :: State -> [Conversation]
conversations s = case selectedChannel s of
  Just chan -> filter (inChannel chan) (s^.stIndex.Index.conversations)
  Nothing   -> []

messages :: State -> [Message]
messages s = case selectedConversation s of
  Just conv -> conv^.convMessages
  Nothing   -> []


-- TODO: Unnecessary boilerplate?

selectedChannelIndex :: State -> Maybe Int
selectedChannelIndex st = do
  chan <- selectedChannel st
  let cs = flattenChannelGroups (channelGroups st)
  elemIndex (Just chan) cs

selectedConversationIndex :: State -> Maybe Int
selectedConversationIndex st = do
  conv <- selectedConversation st
  let cs = st^.to conversations
  elemIndex conv cs

selectedMessageIndex :: State -> Maybe Int
selectedMessageIndex st = do
  msg <- selectedMessage st
  let ms = st^.to messages
  elemIndex msg ms

setSelectedChannel :: (Int -> Int) -> State -> State
setSelectedChannel f st = fromMaybe st st'
  where
    st' = do
      let idx = fromMaybe 0 $ selectedChannelIndex st
      let cs = flattenChannelGroups (channelGroups st)
      let idx' = if f idx >= 0 then f idx else length cs - f idx
      chan <- if idx' > idx
        then find isJust (drop idx' cs)
        else find isJust $ reverse $ take (idx' + 1) cs
      return $ replaceView (mapChan (const chan) (stView st)) st

setSelectedConversation :: (Int -> Int) -> State -> State
setSelectedConversation f st = fromMaybe st st'
  where
    st' = do
      let idx = fromMaybe 0 $ selectedConversationIndex st
      let cs = conversations st
      let idx' = if f idx >= 0 then f idx else length cs - f idx
      conv <- find (const True) (drop idx' cs)
      return $ replaceView (mapConv (const (Just conv)) (stView st)) st

setSelectedMessage :: (Int -> Int) -> State -> State
setSelectedMessage f st = fromMaybe st st'
  where
    st' = do
      let idx = fromMaybe 0 $ selectedMessageIndex st
      let ms = messages st
      let idx' = if f idx >= 0 then f idx else length ms - f idx
      msg <- find (const True) (drop idx' ms)
      return $ replaceView (mapMsg (const (Just msg)) (stView st)) st
