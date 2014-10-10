{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Cli where

import Control.Monad (mapM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)

import Network.Mail.SocialMail.Internal
import Network.Mail.SocialMail.Notmuch (Database, ThreadId)

type ChannelId = Text

type Channels = List (Maybe ChannelId) FormattedText
type Threads  = List ThreadId FormattedText

ui :: Database -> IO ()
ui db = do
  channels <- newList selectedItem 1
  messages <- newList selectedItem 2

  layout <- hBox channels messages
  setBoxChildSizePolicy layout (PerChild (BoxFixed 30) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  addToCollection c layout fg

  fg `onKeyPressed` \this key modifiers ->
    if key == KASCII 'q' then
      exitSuccess
    else
      return False

  fg `onKeyPressed` (channelControls channels)

  channels `onSelectionChange` (renderThreads db messages)

  refreshChannels channels
  showWelcome messages

  runUi c defaultContext

selectedItem :: Attr
selectedItem = Attr (SetTo standout) (SetTo bright_cyan) (SetTo black)

refreshChannels :: Widget Channels -> IO ()
refreshChannels channels = do
  addrs <- getListAddrs
  widgets <- mapM plainText ("all" : "" : addrs)
  let ids = Nothing : Nothing : map Just addrs
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

renderThreads :: Database
              -> Widget Threads
              -> SelectionEvent (Maybe ChannelId) b
              -> IO ()
renderThreads db messages (SelectionOn 0 _ _) = do
  

renderThreads db messages (SelectionOn _ (Just channel) _) = do
  ts <- getThreads db channel
  clearList messages
  mapM_ (uncurry (renderThread messages)) ts

  -- ws <- mapM (uncurry renderThread) ts
  -- forM_ (zip (map fst ts) ws) (uncurry (addToList messages))
renderThreads _ _ (SelectionOn _ Nothing _) = return ()
renderThreads _ _ SelectionOff = return ()

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
