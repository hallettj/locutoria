{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Cli.Widgets where

import           Control.Lens
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import           Graphics.Vty

import           Brick.List
import           Brick.Prim
import           Brick.Border
import           Brick.Util

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.State

data St = St
  { _stUpstreamState :: State
  , _stChannels      :: List (Maybe Channel)
  , _stConversations :: List Conversation
  }

makeLenses ''St

initialSt :: State -> St
initialSt state = St
  { _stUpstreamState = state
  , _stChannels      = list channelListItem []
  , _stConversations = list conversationListItem []
  }


-- top-level views

conversationsView :: St -> [Prim St]
conversationsView st = [channelList st +>> conversationList st]


-- widgets

channelList :: St -> Prim St
channelList _ =
  border unicodeRounded $
    HLimit 25 $ drawList stChannels

channelListItem :: Bool -> Maybe Channel -> Prim (List (Maybe Channel))
channelListItem sel chan = listItem sel $ Txt (unpack (channelDisplay chan))

channelDisplay :: Maybe Channel -> Text
channelDisplay Nothing                = ""
channelDisplay (Just FlaggedChannel)  = "Flagged"
channelDisplay (Just NoListChannel)   = "Direct"
channelDisplay (Just (ListChannel l)) = _mlId l

conversationList :: St -> Prim St
conversationList _ = drawList stConversations

conversationListItem :: Bool -> Conversation -> Prim (List Conversation)
conversationListItem sel conv = listItem sel $ Txt (unpack (fromMaybe "" (conv^.convSubject)))

listItem :: Bool -> Prim (List e) -> Prim (List e)
listItem sel widget =
  let selAttr = white `on` blue
      maybeSelect = if sel then UseAttr selAttr else id
  in maybeSelect widget
