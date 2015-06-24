{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Channel where

import           Control.Lens
import           Data.List (intercalate)
import           Data.Maybe (isNothing)
import           Data.Text (Text)

import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.MailingList

data ChannelGroup = ChannelGroup
  { _groupHeading  :: Text
  , _groupChannels :: [Channel]
  }
  deriving (Eq, Ord, Show)

data Channel = FlaggedChannel
             | ListChannel MailingList
             | NoListChannel
  deriving (Eq, Ord, Show)

makeLenses ''ChannelGroup

inChannel :: Channel -> Conversation -> Bool
inChannel NoListChannel c    = isNothing (c^.convList)
inChannel FlaggedChannel c   = tagged "flagged" c
inChannel (ListChannel ml) c = (c & preview (convList . traverse . mlId)) == Just (ml^.mlId)

-- Effectively flattens list of channel groups into list of channels with a gap
-- between each group of channels. `Maybe` is used so that the gap can be
-- represented with `Nothing`.
flattenChannelGroups :: [ChannelGroup] -> [Maybe Channel]
flattenChannelGroups groups =
  let chanLists  = map (map Just . (^.groupChannels)) groups
  in  intercalate [Nothing] chanLists

