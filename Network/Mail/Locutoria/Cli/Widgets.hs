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
import           Brick.List
import           Brick.Render
import           Brick.Border
import           Brick.Util

import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.State

data St = St
  { _stUpstreamState :: State
  , _stNextAction    :: Maybe (St -> IO ())
  , _stScreenSize    :: (Int, Int)
  , _stChannels      :: List (Maybe Channel)
  , _stConversations :: List Conversation
  , _stMessages      :: List Message
  }

makeLenses ''St

initialSt :: State -> St
initialSt state = St
  { _stUpstreamState = state
  , _stNextAction    = Nothing
  , _stScreenSize    = (80, 50)
  , _stChannels      = list (Name "channels")      channelListItem []
  , _stConversations = list (Name "conversations") conversationListItem []
  , _stMessages      = list (Name "messages")      messageListItem []
  }


-- top-level views

channelView :: St -> [Render]
channelView st = [channelList st <<+ conversationList st]

conversationView :: St -> [Render]
conversationView st = [channelList st <+> messageList st]


-- widgets

channelList :: St -> Render
channelList st =
  border $ hLimit 45 $ vLimit (st^.stScreenSize._2 - 2) $ renderList (st^.stChannels)

channelListItem :: Bool -> Maybe Channel -> Render
channelListItem _ chan = txt (channelDisplay chan)

channelDisplay :: Maybe Channel -> Text
channelDisplay Nothing                = ""
channelDisplay (Just FlaggedChannel)  = "Flagged"
channelDisplay (Just NoListChannel)   = "Direct"
channelDisplay (Just (ListChannel l)) = _mlId l

conversationList :: St -> Render
conversationList st = renderList $ st^.stConversations

conversationListItem :: Bool -> Conversation -> Render
conversationListItem _ conv = txt (fromMaybe "" (conv^.convSubject))

messageList :: St -> Render
messageList st = renderList $ st^.stMessages

messageListItem :: Bool -> Message -> Render
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

messageContent :: Int -> Message -> Render
messageContent w msg | Text.null content = txt "(no content)"
                     | otherwise         = txtArea w content
  where
    content = msgText msg


-- helpers

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr
  [ (listSelectedAttr, white `on` blue)
  ]

txtArea :: Int -> Text -> Render
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
