{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Event
  ( Event
  , Handler
  , handle
  , composeReply
  , enqueue
  , genericError
  , nextChannel
  , prevChannel
  , nextConv
  , prevConv
  , nextMsg
  , prevMsg
  , popView
  , quit
  , refresh
  , showConv
  , setChannel
  , setConv
  , setMsg
  ) where

import           Control.Lens hiding (Index)
import           Data.Text (Text)
import           Network.Mail.Mime (Mail)

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.Channel
import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.State hiding (popView)
import qualified Network.Mail.Locutoria.State as State
import           Network.Mail.Locutoria.View

newtype Event = Event { handle :: State -> (SideEffect, State) }

type Handler e  = e -> IO ()
type SideEffect = Handler Event -> IO ()

-- TODO:
type Activity = Mail


-- events

composeReply :: Event
composeReply = withConv $ \chan conv -> State.pushView $ ComposeReply chan conv

refresh :: Event
refresh = Event $ \s ->
  if not (s^.stRefreshing)
  then (\fire -> (do
    cs <- fetchRecentConversations (s^.stDatabase)
    fire $ indexUpdate cs
    fire $ doneRefresh
    ), s & stRefreshing .~ True
         & stStatus     .~ Refreshing)
  else
    (noSideEffect, s)

doneRefresh :: Event
doneRefresh = pureEvent $ (stStatus .~ Nominal)
                        . (stRefreshing .~ False)

indexUpdate :: (Index -> Index) -> Event
indexUpdate f = pureEvent $ stIndex %~ f

nextChannel, prevChannel :: Event
nextChannel = pureEvent $ selectedChannelIndex %~ (+1)
prevChannel = pureEvent $ selectedChannelIndex %~ (\i -> i - 1)

nextConv, prevConv :: Event
nextConv = pureEvent $ selectedConversationIndex %~ (+1)
prevConv = pureEvent $ selectedConversationIndex %~ (\i -> i - 1)

nextMsg, prevMsg :: Event
nextMsg = pureEvent $ selectedMessageIndex %~ (+1)
prevMsg = pureEvent $ selectedMessageIndex %~ (\i -> i - 1)

popView :: Event
popView = pureEvent $ State.popView

enqueue :: Activity -> Event
enqueue m = undefined  -- TODO

genericError :: Text -> Event
genericError t = pureEvent $ stStatus .~ GenericError t

quit :: Event
quit = pureEvent $ State.pushView Quit

setChannel :: Int -> Event
setChannel i = pureEvent $ selectedChannelIndex .~ i

setConv :: Int -> Event
setConv i = pureEvent $ selectedConversationIndex .~ i

setMsg :: Int -> Event
setMsg i = pureEvent $ selectedMessageIndex .~ i

showConv :: Event
showConv = withConv $ \chan conv -> State.pushView $ ShowConversation chan conv Nothing


-- helpers

pureEvent :: (State -> State) -> Event
pureEvent f = Event $ \st -> (noSideEffect, f st)

withConv :: (Channel -> Conversation -> State -> State) -> Event
withConv f = Event $ \st ->
  case (st ^? selectedChannel, st ^? selectedConversation) of
    (Just chan, Just conv) -> (noSideEffect, f chan conv st)
    (Just _, Nothing)      -> (($ genericError "no conversation selected"), st)
    (Nothing, _)           -> (($ genericError "no channel selected"), st)

noSideEffect :: SideEffect
noSideEffect = const (return ())
