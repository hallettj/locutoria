{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli where

import Control.Event.Handler (Handler)
import           Control.Lens ((^.))
import           Control.Monad (when)
import           Data.IORef
import qualified Data.Map as Map
import Data.Text (append, pack)
import Graphics.Vty.Input (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All hiding (Handler)
import System.Exit (exitSuccess)

import qualified Network.Mail.Locutoria.Client as Client
import qualified Network.Mail.Locutoria.Compose as C
import Network.Mail.Locutoria.Keymap
import Network.Mail.Locutoria.Internal (ChannelId, ThreadInfo)
import Network.Mail.Locutoria.MailingList (MailingList(..))
import Network.Mail.Locutoria.Notmuch (SearchTerm, ThreadId)
import Network.Mail.Locutoria.State

type Channels = List (Maybe ChannelId) FormattedText
type Threads  = List ThreadId FormattedText

data Cli = Cli
  { _collection :: Collection
  , _channels   :: Widget Channels
  , _threads    :: Widget Threads
  }

ui :: KeyBindings -> Handler Client.Event -> State -> IO Client.Ui
ui kb fire upstreamState = do
  stateRef <- newIORef upstreamState
  cli      <- initUi kb fire stateRef
  let update = onStateChange fire cli stateRef
  let run    = resumeUi cli >> fire Client.Refresh
  return $ Client.Ui run update

initUi :: KeyBindings -> Handler Client.Event -> IORef State -> IO Cli
initUi kb fire stateRef = do
  channels <- newList 1 :: IO (Widget Channels)
  threads <- newList 2  :: IO (Widget Threads)

  layout <- hBox channels threads
  setBoxChildSizePolicy layout (PerChild (BoxFixed 30) BoxAuto)

  fg <- newFocusGroup
  _  <- addToFocusGroup fg threads

  c <- newCollection
  _ <- addToCollection c layout fg

  fg `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire this key modifiers (keymapGlobal kb)

  threads `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire this key modifiers (keymapConvList kb)

  channels `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire this key modifiers (keymapChanList kb)

  return $ Cli
    { _collection = c
    , _channels   = channels
    , _threads    = threads
    }

resumeUi :: Cli -> IO ()
resumeUi cli = runUi (_collection cli) defaultContext

onStateChange :: Handler Client.Event
              -> Cli
              -> IORef State
              -> State
              -> IO ()
onStateChange fire cli stateRef state = do
  prevState <- readIORef stateRef
  writeIORef stateRef state

  when (prevState^.channels /= state^.channels) $
    renderChannels (_channels cli) (state^.channels)
  case currentChannelIndex state of
    Just n  -> setSelected (_channels cli) (n + 2)
    Nothing -> return ()

  when (prevState^.conversations /= state^.conversations) $
    renderThreads  (_threads  cli) (state^.conversations)

  case state^.activity of
    Shutdown -> shutdownUi >> exitSuccess
    _ -> return ()

handleKey :: IORef State
          -> Handler Client.Event
          -> Widget a
          -> Key -> [Modifier]
          -> Keymap
          -> IO Bool
handleKey stateRef fire _ key mods keymap = do
  upstreamState <- readIORef stateRef
  case Map.lookup (key, mods) keymap of
    Just act -> act fire upstreamState >> return True
    Nothing  -> return False

renderChannels :: Widget Channels -> [MailingList] -> IO ()
renderChannels channels ls = do
  let mlIds = map mlId ls
  widgets <- mapM plainText ("all" : "" : mlIds)
  let ids = Nothing : Nothing : map Just mlIds
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
