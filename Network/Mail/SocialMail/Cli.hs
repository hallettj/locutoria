{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.SocialMail.Cli where

import Control.Event.Handler (AddHandler, newAddHandler)
import Control.Monad (mapM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All
import Reactive.Banana (Moment, compile, filterE, mapAccum)
import Reactive.Banana.Frameworks (Frameworks, actuate, fromAddHandler, reactimate)
import System.Exit (exitSuccess)

import Network.Mail.SocialMail.Client
import Network.Mail.SocialMail.Internal (ChannelId)
import Network.Mail.SocialMail.Notmuch (ThreadId)

type Channels = List (Maybe ChannelId) FormattedText
type Threads  = List ThreadId FormattedText

ui :: ClientConfig -> IO ()
ui config = do
  channels <- newList selectedItem 1
  threads <- newList selectedItem 2
  showWelcome threads

  layout <- hBox channels threads
  setBoxChildSizePolicy layout (PerChild (BoxFixed 30) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  addToCollection c layout fg

  (addClientEvent, fireClientEvent) <- newAddHandler

  fg `onKeyPressed` \this key modifiers ->
    case globalControls this key modifiers of
      Just ev -> fireClientEvent ev >> return True
      Nothing -> return False

  fg `onKeyPressed` \this key modifiers ->
    case channelControls this key modifiers of
      Just ev -> fireClientEvent ev >> return True
      Nothing -> return False

  fg `onKeyPressed` (channelControls_ channels)

  channels `onSelectionChange` \e ->
    case channelSelection e of
      Just ev -> fireClientEvent ev
      Nothing -> return ()

  network <- compile $
    networkDescription addClientEvent fireClientEvent config channels threads
  actuate network

  fireClientEvent GetChannels

  runUi c defaultContext

networkDescription :: forall t. Frameworks t => AddHandler ClientEvent
                                       -> (ClientEvent -> IO ())
                                       -> ClientConfig
                                       -> Widget Channels
                                       -> Widget Threads
                                       -> Moment t ()
networkDescription addEvent fire config channels threads = do
  clientEvents <- fromAddHandler addEvent
  let
    responseActions = fmap (step config) clientEvents
    (responses, state) = mapAccum initState responseActions
  reactimate $ fmap (\e -> case e of
    DataResp e' -> stepData fire e'
    UiResp e' -> stepUi channels threads e'
    ) responses

stepUi :: Widget Channels -> Widget Threads -> UiEvent -> IO ()
stepUi channels threads e = case e of
  RenderChannels cs -> renderChannels channels cs
  RenderThreads ts -> renderThreads threads ts
  UiExit -> exitSuccess
  UiNoop -> return ()

selectedItem :: Attr
selectedItem = Attr (SetTo standout) (SetTo bright_cyan) (SetTo black)

globalControls :: Widget a -> Key -> [Modifier] -> Maybe ClientEvent
globalControls _ key mods =
    if key == KASCII 'q' then
      Just ClientExit
    else
      Nothing

channelControls :: Widget a -> Key -> [Modifier] -> Maybe ClientEvent
channelControls _ key mods =
  if key == KASCII '@' then
    Just GetChannels
  else
    Nothing

channelControls_ :: Widget Channels -> Widget a -> Key -> [Modifier] -> IO Bool
channelControls_ channels _ key mods =
  if key == KASCII 'p' && MCtrl `elem` mods then
    do
      sel <- getSelected channels
      let n  = fromMaybe 1 (fmap fst sel)
      let n' = if n - 1 == 1 then 0 else n - 1
      setSelected channels n'
      return True
  else if key == KASCII 'n' && MCtrl `elem` mods then
    do
      sel <- getSelected channels
      let n  = fromMaybe 1 (fmap fst sel)
      let n' = if n + 1 == 1 then 2 else n + 1
      setSelected channels n'
      return True
  else
    return False

channelSelection :: SelectionEvent (Maybe ChannelId) b -> Maybe ClientEvent
channelSelection (SelectionOn 0 _ _) = Just GetAllThreads
channelSelection (SelectionOn _ channel _) = fmap GetThreads channel

renderChannels :: Widget Channels -> [Text] -> IO ()
renderChannels channels cs = do
  widgets <- mapM plainText ("all" : "" : cs)
  let ids = Nothing : Nothing : map Just cs
  clearList channels
  mapM_ (uncurry (addToList channels)) (zip ids widgets)

renderThreads :: Widget Threads -> [(ThreadId, Text)] -> IO ()
renderThreads threads ts = do
  clearList threads
  mapM_ (uncurry (renderThread threads)) ts

renderThread :: Widget Threads
             -> ThreadId -> Text
             -> IO ()
renderThread threads tid summary = do
  t <- plainText summary
  addToList threads tid t

showWelcome :: Widget Threads -> IO ()
showWelcome threads = do
  t <- plainText "Welcome to your email.  Use Ctrl+P and Ctrl+N to change channels."
  addToList threads "" t
