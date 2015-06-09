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
  , _stChannels      = list channelListItem []
  , _stConversations = list conversationListItem []
  , _stMessages      = list messageListItem []
  }


-- top-level views

channelView :: St -> [Render St]
channelView st = [channelList st <<+ conversationList st]

conversationView :: St -> [Render St]
conversationView st = [channelList st <+> messageList st]


-- widgets

channelList :: St -> Render St
channelList st =
  border unicode $ hLimit 45 $ vLimit (st^.stScreenSize._2 - 2) $ withLens stChannels drawList

channelListItem :: Bool -> Maybe Channel -> Render (List (Maybe Channel))
channelListItem sel chan = listItem sel $ txt (unpack (channelDisplay chan))

channelDisplay :: Maybe Channel -> Text
channelDisplay Nothing                = ""
channelDisplay (Just FlaggedChannel)  = "Flagged"
channelDisplay (Just NoListChannel)   = "Direct"
channelDisplay (Just (ListChannel l)) = _mlId l

conversationList :: St -> Render St
conversationList _ = withLens stConversations drawList

conversationListItem :: Bool -> Conversation -> Render (List Conversation)
conversationListItem sel conv = listItem sel $ txt (unpack (fromMaybe "" (conv^.convSubject)))

messageList :: St -> Render St
messageList _ = withLens stMessages drawList

messageListItem :: Bool -> Message -> Render (List Message)
messageListItem sel msg = listItem sel $
  border unicode $ hLimit 100 $
    txt (author <> "  —  " <> date)
    <=>
    hBorder unicode
    <=>
    messageContent 80 msg
  where
    author  = unpack $ fromMaybe "(unknown author)" $ msgAuthor msg
    date    = unpack $ msg^.msgDateRelative

messageContent :: Int -> Message -> Render a
messageContent w msg | null content = txt "(no content)"
                     | otherwise    = txtArea w content
  where
    content = unpack $ msgText msg


-- helpers

listItem :: Bool -> Render (List e) -> Render (List e)
listItem sel widget =
  let selAttr = white `on` blue
      maybeSelect = if sel then useAttr selAttr else id
  in maybeSelect widget

txtArea :: Int -> String -> Render a
txtArea w s =
  let bf = BreakFormat
        { bfMaxCol       = w
        , bfTabRep       = 4
        , bfHyphenSymbol = '-'
        , bfHyphenator   = Nothing
        }
      ls = breakStringLn bf s
  in foldl1' (<=>) $ map txt ls
