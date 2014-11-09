{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Cli where

import Control.Event.Handler (Handler)
import Control.Monad (mapM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, append, pack)
import qualified Data.Text.IO as TIO
import Graphics.Vty.Attributes
import Graphics.Vty.Input (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All hiding (Handler)
import System.Exit (exitSuccess)

import Network.Mail.SocialMail.Client
import Network.Mail.SocialMail.Internal (ChannelId, ThreadInfo)
import Network.Mail.SocialMail.Notmuch (ThreadId)

type Channels = List (Maybe ChannelId) FormattedText
type Threads  = List ThreadId FormattedText

ui :: ClientConfig -> Handler ClientEvent -> IO (Handler UiEvent)
ui config fireClientEvent = do
  channels <- newList 1
  threads <- newList 2
  showWelcome threads

  layout <- hBox channels threads
  setBoxChildSizePolicy layout (PerChild (BoxFixed 30) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg threads

  c <- newCollection
  addToCollection c layout fg

  fg `onKeyPressed` \this key modifiers ->
    case globalControls this key modifiers of
      Just ev -> fireClientEvent ev >> return True
      Nothing -> return False

  fg `onKeyPressed` \this key modifiers ->
    case channelControls this key modifiers of
      Just ev -> fireClientEvent ev >> return True
      Nothing -> return False

  fg `onKeyPressed` (channelControls_ channels)
  fg `onKeyPressed` (threadControls_ threads)

  channels `onSelectionChange` \e ->
    case channelSelection e of
      Just ev -> fireClientEvent ev
      Nothing -> return ()

  runUi c defaultContext

  return $ stepUi channels threads

stepUi :: Widget Channels -> Widget Threads -> Handler UiEvent
stepUi channels threads e = case e of
  RenderChannels cs -> renderChannels channels cs
  RenderThreads ts -> renderThreads threads ts
  UiExit -> exitSuccess

globalControls :: Widget a -> Key -> [Modifier] -> Maybe ClientEvent
globalControls _ key mods =
    if key == KChar 'q' then
      Just ClientExit
    else
      Nothing

channelControls :: Widget a -> Key -> [Modifier] -> Maybe ClientEvent
channelControls _ key mods =
  if key == KChar '@' then
    -- Just GetChannels
    Just GetLikeCounts
  else
    Nothing

channelControls_ :: Widget Channels -> Widget a -> Key -> [Modifier] -> IO Bool
channelControls_ channels _ key mods = case (key, mods) of
  (KChar 'p', [MCtrl]) -> do
    sel <- getSelected channels
    let n  = fromMaybe 1 (fmap fst sel)
    let n' = if n - 1 == 1 then 0 else n - 1
    setSelected channels n'
    return True
  (KChar 'n', [MCtrl]) -> do
    sel <- getSelected channels
    let n  = fromMaybe 1 (fmap fst sel)
    let n' = if n + 1 == 1 then 2 else n + 1
    setSelected channels n'
    return True
  _ ->
    return False

threadControls_ :: Widget Threads -> Widget a -> Key -> [Modifier] -> IO Bool
threadControls_ threads _ key mods = case (key, mods) of
  (KChar 'j', []) -> do
      sel <- getSelected threads
      let n = fromMaybe (-1) (fmap fst sel)
      setSelected threads (n + 1)
      return True
  (KChar 'k', []) -> do
      sel <- getSelected threads
      let n = fromMaybe 1 (fmap fst sel)
      setSelected threads (n - 1)
      return True
  (KChar 'g', []) -> do
      setSelected threads 0
      return True
  (KChar 'G', _) -> do
      len <- getListSize threads
      setSelected threads (len - 1)
      return True
  _ ->
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

renderThreads :: Widget Threads -> [ThreadInfo] -> IO ()
renderThreads threads ts = do
  clearList threads
  mapM_ (renderThread threads) ts

renderThread :: Widget Threads -> ThreadInfo -> IO ()
renderThread threads (tid, summary, likes, liked) = do
  let wlikes = if likes > 0 then summary `append` likeCount else summary
  let wlikes' = if liked then wlikes `append` " ↑" else wlikes
  t <- plainText wlikes'
  addToList threads tid t
  where
    likeCount = pack ("\n+" ++ show likes)

showWelcome :: Widget Threads -> IO ()
showWelcome threads = do
  t <- plainText "Welcome to your email.  Use Ctrl+P and Ctrl+N to change channels."
  addToList threads "" t
