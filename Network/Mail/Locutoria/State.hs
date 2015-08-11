{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.State where

import           Control.Applicative ((<$>), pure)
import           Control.Lens hiding (Index)
import           Data.Default (def)
import           Data.List (elemIndex, find)
import           Data.Maybe (catMaybes, isJust, listToMaybe)
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

stChannel :: Traversal' State Channel
stChannel = stView . viewChannel

stConversation :: Traversal' State Conversation
stConversation f st = case conv of
  Nothing -> pure st
  Just c  -> updateConv <$> f c
  where
    conv = case st ^. stView of
      ShowChannel _ Nothing -> listToMaybe (stConversations st)
      v -> v ^? viewConversation

    updateConv c = case st ^. stView of
      ShowChannel chan Nothing -> st & stView .~ ShowChannel chan (Just c)
      _ -> st & stView . viewConversation .~ c

stMessage :: Traversal' State Message
stMessage f st = case msg of
  Nothing -> pure st
  Just m  -> updateMsg <$> f m
  where
    msg = case st ^. stView of
      ShowConversation _ _ Nothing -> listToMaybe (stMessages st)
      v -> v ^? viewMessage

    updateMsg m = case st ^. stView of
      ShowConversation chan conv Nothing -> st & stView .~ ShowConversation chan conv (Just m)
      _ -> st & stView . viewMessage .~ m

stChannelGroups :: State -> [ChannelGroup]
stChannelGroups s =
  [ ChannelGroup "Direct"  [NoListChannel]
  , ChannelGroup "Flagged" [FlaggedChannel]
  , ChannelGroup "Lists"   ls
  ]
  where
    ls = ListChannel <$> lists (s^.stIndex)

stChannels :: State -> [Channel]
stChannels st = catMaybes $ flattenChannelGroups (stChannelGroups st)

stConversations :: State -> [Conversation]
stConversations st = case st ^? stChannel of
  Just chan -> filter (inChannel chan) (st^.stIndex.Index.conversations)
  Nothing   -> []

stMessages :: State -> [Message]
stMessages st = case st ^? stConversation of
  Just conv -> conv^.convMessages
  Nothing   -> []

stChannelIndex :: Traversal' State Int
stChannelIndex f st = case (idx, chan) of
  (_, Nothing) -> pure st
  (Nothing, _) -> pure st
  (Just i,  _) -> update <$> f i
    where
      update i' = case clampedLookup isJust i i' cs of
        Just (Just chan') -> st & stChannel .~ chan'
        _                 -> st
  where
    chan = st ^? stChannel
    cs   = flattenChannelGroups $ stChannelGroups st
    idx  = flip elemIndex cs . Just =<< chan

stConversationIndex :: Traversal' State Int
stConversationIndex f st = case idx of
  Nothing -> pure st
  Just i  -> update . clamp cs <$> f i
  where
    cs  = st^.to stConversations
    idx = flip elemIndex cs =<< st ^? stConversation
    update idx' = case find (const True) (drop idx' cs) of
      Just conv -> st & stConversation .~ conv
      Nothing   -> st

stMessageIndex :: Traversal' State Int
stMessageIndex f st = case idx of
  Nothing -> pure st
  Just i  -> update . clamp ms <$> f i
  where
    ms  = stMessages st
    idx = flip elemIndex ms =<< st ^? stMessage
    update idx' = case find (const True) (drop idx' ms) of
      Just msg -> st & stMessage .~ msg
      Nothing  -> st

clamp :: [a] -> Int -> Int
clamp xs i = if i >= 0 then i `min` end else length xs - i `max` 0
  where
    end = length xs - 1

clampedLookup :: (a -> Bool) -> Int -> Int -> [a] -> Maybe a
clampedLookup p oldIdx newIdx xs = find p xs'
  where
    idx = if newIdx >= 0 && newIdx < oldIdx
          then 0 - (length xs - newIdx)
          else newIdx
    xs' = if idx >= 0
          then drop idx xs
          else drop (0 - idx - 1) (reverse xs)
