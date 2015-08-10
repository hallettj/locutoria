{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Cli.Widgets where

import           Control.Lens
import           Data.List (foldl1')
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty
import           Network.Mail.Mime (Address(..))
import           Text.LineBreak

import           Brick.AttrMap
import           Brick.Core
import           Brick.Util
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Brick.Widgets.List

import           Network.Mail.Locutoria.Channel
import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.State

-- top-level views

channelView :: State -> [Widget]
channelView st = layout st (channelList st <+> conversationList st)

conversationView :: State -> [Widget]
conversationView st = layout st (channelList st <+> messageList st)

-- layout

layout :: State -> Widget -> [Widget]
layout st mainWidget = [mainWidget <=> statusLine st]

-- widgets

channelList :: State -> Widget
channelList st =
  border $ hLimit 45 $ vLimit (st^.stScreenSize._2 - 2) $ renderList $
    listMoveTo selected $ channels
  where
    channels = list (Name "channels") channelListItem (flattenChannelGroups (st^.to channelGroups))
    selected = fromMaybe 0 $ st ^? selectedChannelIndex

channelListItem :: Bool -> Maybe Channel -> Widget
channelListItem _ chan = txt (channelDisplay chan)

channelDisplay :: Maybe Channel -> Text
channelDisplay Nothing                = ""
channelDisplay (Just FlaggedChannel)  = "Flagged"
channelDisplay (Just NoListChannel)   = "Direct"
channelDisplay (Just (ListChannel l)) = _mlId l

conversationList :: State -> Widget
conversationList st = renderList $ listMoveTo selected $ convs
  where
    convs = list (Name "conversations") conversationListItem (st^.to conversations)
    selected = fromMaybe 0 $ st ^? selectedConversationIndex

conversationListItem :: Bool -> Conversation -> Widget
conversationListItem _ conv = txt (fromMaybe "" (conv^.convSubject))

messageList :: State -> Widget
messageList st = renderList msgs
  where
    msgs = list (Name "messages") messageListItem (st^.to messages)

messageListItem :: Bool -> Message -> Widget
messageListItem _ msg =
  border $ hLimit 100 $
    txt (author <> "  —  " <> date)
    <=>
    hBorder
    <=>
    messageContent 80 msg
  where
    author  = either (const "(unknown author)") id $ fmap showAddress $ msgFrom msg
    date    = msg^.msgDateRelative

messageContent :: Int -> Message -> Widget
messageContent w msg | Text.null content = txt "(no content)"
                     | otherwise         = txtArea w content
  where
    content = msgText msg

statusLine :: State -> Widget
statusLine st = txt content
  where
    content = case st^.stStatus of
      GenericError t -> "Error: " <> t
      Refreshing     -> "Refreshing..."
      Nominal        -> ""


-- helpers

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr
  [ (listSelectedAttr, white `on` blue)
  ]

txtArea :: Int -> Text -> Widget
txtArea w s =
  let bf = BreakFormat
        { bfMaxCol       = w
        , bfTabRep       = 4
        , bfHyphenSymbol = '-'
        , bfHyphenator   = Nothing
        }
      ls = map Text.pack $ breakStringLn bf (Text.unpack s)
  in foldl1' (<=>) $ map txt ls

showAddress :: Address -> Text
showAddress (Address (Just name) addr) = mconcat [name, " <", addr, ">"]
showAddress (Address Nothing addr) = addr
