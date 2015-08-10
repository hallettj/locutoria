{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.State where

import           Control.Applicative ((<$>), pure)
import           Control.Lens hiding (Index)
import           Data.Default (def)
import           Data.List (elemIndex, find)
import           Data.Maybe (fromJust, fromMaybe, isJust)
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

stView :: Lens' State View
stView f st = case st^.stHistory of
  v:vs -> (\v' ->  st & stHistory .~ (v':vs)) <$> f v
  []   -> (\v' ->  st & stHistory .~ (v':[])) <$> f Root

pushView :: View -> State -> State
pushView v = stHistory %~ (v:)

popView :: State -> State
popView = stHistory %~ drop 1

replaceView :: View -> State -> State
replaceView v = pushView v . popView

selectedChannel :: Traversal' State Channel
selectedChannel = stView . viewChannel

selectedConversation :: Traversal' State Conversation
selectedConversation = stView . viewConversation

selectedMessage :: Traversal' State Message
selectedMessage = stView . viewMessage

channelGroups :: State -> [ChannelGroup]
channelGroups s =
  [ ChannelGroup "Direct"  [NoListChannel]
  , ChannelGroup "Flagged" [FlaggedChannel]
  , ChannelGroup "Lists"   ls
  ]
  where
    ls = ListChannel <$> lists (s^.stIndex)

conversations :: State -> [Conversation]
conversations st = case st ^? selectedChannel of
  Just chan -> filter (inChannel chan) (st^.stIndex.Index.conversations)
  Nothing   -> []

messages :: State -> [Message]
messages st = case st ^? selectedConversation of
  Just conv -> conv^.convMessages
  Nothing   -> []

selectedChannelIndex :: Traversal' State Int
selectedChannelIndex f st = case st ^? selectedChannel of
  Nothing   -> pure st
  Just chan ->
    let cs  = flattenChannelGroups (channelGroups st)
        idx = fromJust $ elemIndex (Just chan) cs
        update idx' =
          let chan' = if idx' > idx
                      then find isJust (drop idx' cs)
                      else find isJust $ reverse $ take (idx' + 1) cs
          in st & selectedChannel .~ (fromJust (fromJust chan'))
    in update . clamp cs <$> f idx

selectedConversationIndex :: Traversal' State Int
selectedConversationIndex f st = update . clamp cs <$> f idx
  where
    cs  = st^.to conversations
    idx = fromMaybe 0 $ flip elemIndex cs =<< st ^? selectedConversation
    update idx' = st & selectedConversation .~ conv
      where
        conv = head (drop idx' cs)

selectedMessageIndex :: Traversal' State Int
selectedMessageIndex f st = update . clamp ms <$> f idx
  where
    ms  = messages st
    idx = fromMaybe 0 $ flip elemIndex ms =<< st ^? selectedMessage
    update idx' = st & selectedMessage .~ msg
      where
        msg = head (drop idx' ms)

clamp :: [a] -> Int -> Int
clamp xs i = if i >= 0 then i `max` end else length xs - (i `max` end)
  where
    end = length xs - 1
