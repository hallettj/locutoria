{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.State where

import           Control.Applicative ((<$>))
import           Control.Lens (makeLenses, preview, traverse)
import           Control.Lens.Operators
import           Data.Default (Default, def)
import           Data.Maybe (isNothing)
import           Data.Text (Text)

import           Network.Mail.Locutoria.Index hiding (_conversations, conversations)
import qualified Network.Mail.Locutoria.Index as Index
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.Conversation

data State = State
  { _index           :: Index
  , _refreshing      :: Bool
  , _route           :: Route
  }
  deriving (Eq, Show)

data Route = Root
           | ShowChannel Channel (Maybe Conversation)
           | ShowConversation Channel Conversation
           | ComposeReply Channel Conversation
  deriving (Eq, Ord, Show)

data ChannelGroup = ChannelGroup
  { _groupHeading  :: Text
  , _groupChannels :: [Channel]
  }
  deriving (Eq, Ord, Show)

data Channel = FlaggedChannel
             | ListChannel MailingList
             | NoListChannel
  deriving (Eq, Ord, Show)


instance Default State where
  def = State { _index           = def
              , _refreshing      = False
              , _route           = Root
              }

makeLenses ''State
makeLenses ''ChannelGroup

selectedChannel :: State -> Maybe Channel
selectedChannel state = case state^.route of
  Root                    -> Nothing
  ShowChannel chan _      -> Just chan
  ShowConversation chan _ -> Just chan
  ComposeReply chan _     -> Just chan

selectedConversation :: State -> Maybe Conversation
selectedConversation state = case state^.route of
  Root                    -> Nothing
  ShowChannel _ conv      -> conv
  ShowConversation _ conv -> Just conv
  ComposeReply _ conv     -> Just conv

channelGroups :: State -> [ChannelGroup]
channelGroups s =
  [ ChannelGroup "Direct"  [NoListChannel]
  , ChannelGroup "Flagged" [FlaggedChannel]
  , ChannelGroup "Lists"   ls
  ]
  where
    ls = ListChannel <$> lists (_index s)

conversations :: State -> [Conversation]
conversations s = case selectedChannel s of
  Just chan -> filter (inChannel chan) (s^.index.Index.conversations)
  Nothing   -> []

messages :: State -> [Message]
messages s = case selectedConversation s of
  Just conv -> conv^.convMessages
  Nothing   -> []

inChannel :: Channel -> Conversation -> Bool
inChannel NoListChannel c    = isNothing (c^.convList)
inChannel FlaggedChannel c   = tagged "flagged" c
inChannel (ListChannel ml) c = (c & preview (convList . traverse . mlId)) == Just (ml^.mlId)

tagged :: Text -> Conversation -> Bool
tagged tag c = any (\m -> tag `elem` _msgTags m) (c^.convMessages)
