{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Cli.Widgets where

import           Control.Lens
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import           Graphics.Vty

import           Brick.List
import           Brick.Prim
import           Brick.Border
import           Brick.Util

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.State

data St = St
  { _stUpstreamState :: State
  , _stChannels      :: List (Maybe Channel)
  , _stConversations :: List Conversation
  , _stMessages      :: List Message
  }

makeLenses ''St

initialSt :: State -> St
initialSt state = St
  { _stUpstreamState = state
  , _stChannels      = list channelListItem []
  , _stConversations = list conversationListItem []
  , _stMessages      = list messageListItem []
  }


-- top-level views

channelView :: St -> [Prim St]
channelView st = [channelList st <+> conversationList st]

conversationView :: St -> [Prim St]
conversationView st = [channelList st <+> messageList st]


-- widgets

channelList :: St -> Prim St
channelList _ =
  border unicode $ HLimit 40 $ VLimit 65 $ drawList stChannels

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

messageList :: St -> Prim St
messageList _ = drawList stMessages

messageListItem :: Bool -> Message -> Prim (List Message)
messageListItem sel msg = listItem sel $
  border unicode $ HLimit 100 $
    Txt (author <> "  —  " <> date)
    <=>
    hBorder unicode
    <=>
    Txt content
  where
    author  = unpack $ fromMaybe "(unknown author)" $ msgAuthor msg
    date    = unpack $ msg^.msgDateRelative
    content = unpack $ msgText msg


-- helpers

listItem :: Bool -> Prim (List e) -> Prim (List e)
listItem sel widget =
  let selAttr = white `on` blue
      maybeSelect = if sel then UseAttr selAttr else id
  in maybeSelect widget
