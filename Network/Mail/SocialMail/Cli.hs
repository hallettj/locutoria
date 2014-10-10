{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Cli where

import Control.Event.Handler (Handler)
import Control.Monad (mapM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)

import Network.Mail.SocialMail.Client
import Network.Mail.SocialMail.Internal
import Network.Mail.SocialMail.Notmuch (ThreadId)

type ChannelId = Text

type Channels = List (Maybe ChannelId) FormattedText
type Threads  = List ThreadId FormattedText

ui :: Handler ClientEvent -> IO ()
ui fire = do
  channels <- newList selectedItem 1
  messages <- newList selectedItem 2

  layout <- hBox channels messages
  setBoxChildSizePolicy layout (PerChild (BoxFixed 30) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  addToCollection c layout fg

  let networkDescription :: forall t. Frameworks t => Moment t ()
      networkDescription = do
        ekeyboard <- fromAddHandler $ onKeyPressed fg
        let
          state = accumB initState
        reactimate undefined

  network <- compile networkDescription
  actuate network

  fg `onKeyPressed` \this key modifiers ->
    if key == KASCII 'q' then
      fire Exit >> return True
    else
      return False

  fg `onKeyPressed` (channelControls channels)

  channels `onSelectionChange` (refreshThreads messages)

  refreshChannels channels
  showWelcome messages

  runUi c defaultContext

selectedItem :: Attr
selectedItem = Attr (SetTo standout) (SetTo bright_cyan) (SetTo black)

refreshChannels :: Widget Channels -> IO ()
refreshChannels channels = fire (GetChannels channels)

renderChannels :: Widget Channels -> [Text] -> IO ()
renderChannels channels cs = do
  widgets <- mapM plainText ("all" : "" : cs)
  let ids = Nothing : Nothing : map Just cs
  clearList channels
  mapM_ (uncurry (addToList channels)) (zip ids widgets)

channelControls :: Widget Channels -> Widget c -> Key -> [Modifier] -> IO Bool
channelControls channels this key mods =
  if key == KASCII '@' then
    refreshChannels channels >> return True
  else if key == KASCII 'p' && MCtrl `elem` mods then
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

refreshThreads :: Widget Threads -> SelectionEvent (Maybe ChannelId) b -> IO ()
renderThreads threads (SelectionOn 0 _ _) = fire (GetAllThreads threads)
renderThreads threads (SelectionOn _ (Just channel) _) =
  fire (GetThreads threads channel)
renderThreads _ _ (SelectionOn _ Nothing _) = return ()
renderThreads _ _ SelectionOff = return ()

renderThreads :: Widget Threads -> [(ThreadId, Text)] -> IO ()
renderThreads messages (SelectionOn _ (Just channel) _) = do
  clearList messages
  mapM_ (uncurry (renderThread messages)) ts

renderThread :: Widget Threads
             -> ThreadId -> Text
             -> IO ()
renderThread messages tid summary = do
  t <- plainText summary
  addToList messages tid t

showWelcome :: Widget Threads -> IO ()
showWelcome messages = do
  t <- plainText "Welcome to your email.  Use Ctrl+P and Ctrl+N to change channels."
  addToList messages "" t
