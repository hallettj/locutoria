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
import           Network.Mail.Locutoria.Notmuch hiding (MessageId)

data State = State
  { _index           :: Index
  , _selectedChannel :: Channel
  , _selectedThread  :: Maybe ThreadId
  , _refreshing      :: Bool
  , _route           :: Route
  }
  deriving Show

data Route = Root
           | ShowChannel Channel
           | ShowConversation Channel ThreadId
           | ComposeReply Channel ThreadId
           | Shutdown
  deriving Show

data ChannelGroup = ChannelGroup
  { _groupHeading  :: Text
  , _groupChannels :: [Channel]
  }
  deriving (Eq, Ord, Show)

data Channel = EmptyChannel
             | FlaggedChannel
             | ListChannel MailingList
             | NoListChannel
  deriving (Eq, Ord, Show)


instance Default State where
  def = State { _index           = def
              , _selectedChannel = EmptyChannel
              , _selectedThread  = Nothing
              , _refreshing      = False
              , _route           = Root
              }

makeLenses ''State
makeLenses ''ChannelGroup

channelGroups :: State -> [ChannelGroup]
channelGroups s =
  [ ChannelGroup "Direct"  [NoListChannel]
  , ChannelGroup "Flagged" [FlaggedChannel]
  , ChannelGroup "Lists"   ls
  ]
  where
    ls = ListChannel <$> lists (_index s)

conversations :: State -> [Conversation]
conversations s = filter (inChannel (s^.selectedChannel)) (s^.index.Index.conversations)

inChannel :: Channel -> Conversation -> Bool
inChannel EmptyChannel _     = False
inChannel NoListChannel c    = isNothing (c^.list)
inChannel FlaggedChannel c   = tagged "flagged" c
inChannel (ListChannel ml) c = (c & preview (list . traverse . mlId)) == Just (ml^.mlId)

tagged :: Text -> Conversation -> Bool
tagged tag c = any (\m -> tag `elem` _msgTags m) (c^.messages)
