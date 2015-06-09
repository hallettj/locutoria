{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Cli.Widgets where

import           Control.Lens
import           Data.List (foldl1')
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import           Graphics.Vty
import           Text.LineBreak

import           Brick.Core
import           Brick.List
import           Brick.Render
import           Brick.Border
import           Brick.Util

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.State

data St = St
  { _stUpstreamState :: State
  , _stVty           :: Maybe Vty
  , _stNextAction    :: IO ()
  , _stScreenSize    :: (Int, Int)
  , _stChannels      :: List (Maybe Channel)
  , _stConversations :: List Conversation
  , _stMessages      :: List Message
  }

makeLenses ''St

initialSt :: State -> St
initialSt state = St
  { _stUpstreamState = state
  , _stVty           = Nothing
  , _stNextAction    = return ()
  , _stScreenSize    = (80, 50)
  , _stChannels      = list (Name "channels")      channelListItem []
  , _stConversations = list (Name "conversations") conversationListItem []
  , _stMessages      = list (Name "messages")      messageListItem []
  , _stEditing       = False
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
channelListItem sel chan = listItem sel $ txt (unpack (channelDisplay chan))

channelDisplay :: Maybe Channel -> Text
channelDisplay Nothing                = ""
channelDisplay (Just FlaggedChannel)  = "Flagged"
channelDisplay (Just NoListChannel)   = "Direct"
channelDisplay (Just (ListChannel l)) = _mlId l

conversationList :: St -> Render
conversationList st = renderList $ st^.stConversations

conversationListItem :: Bool -> Conversation -> Render
conversationListItem sel conv = listItem sel $ txt (unpack (fromMaybe "" (conv^.convSubject)))

messageList :: St -> Render
messageList st = renderList $ st^.stMessages

messageListItem :: Bool -> Message -> Render
messageListItem sel msg = listItem sel $
  border $ hLimit 100 $
    txt (author <> "  —  " <> date)
    <=>
    hBorder
    <=>
    messageContent 80 msg
  where
    author  = unpack $ fromMaybe "(unknown author)" $ msgAuthor msg
    date    = unpack $ msg^.msgDateRelative

messageContent :: Int -> Message -> Render
messageContent w msg | null content = txt "(no content)"
                     | otherwise    = txtArea w content
  where
    content = unpack $ msgText msg


-- helpers

listItem :: Bool -> Render -> Render
listItem sel widget =
  let selAttr = white `on` blue
      maybeSelect = if sel then withAttr selAttr else id
  in maybeSelect widget

txtArea :: Int -> String -> Render
txtArea w s =
  let bf = BreakFormat
        { bfMaxCol       = w
        , bfTabRep       = 4
        , bfHyphenSymbol = '-'
        , bfHyphenator   = Nothing
        }
      ls = breakStringLn bf s
  in foldl1' (<=>) $ map txt ls
