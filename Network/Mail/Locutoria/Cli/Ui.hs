{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (Handler)
import           Control.Lens ((^.), to)
import           Control.Monad (when)
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Graphics.Vty.Input (Key(..), Modifier(..))
import           Graphics.Vty.Widgets.All hiding (Handler, state)
import           System.Exit (exitSuccess)

import           Network.Mail.Locutoria.Cli
import           Network.Mail.Locutoria.Cli.Keymap
import qualified Network.Mail.Locutoria.Client as Client
import qualified Network.Mail.Locutoria.Compose as C
import           Network.Mail.Locutoria.MailingList (MailingList(..))
import           Network.Mail.Locutoria.Index hiding (_conversations, conversations)
import           Network.Mail.Locutoria.State

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

  let cli = Cli { _collection = c, _channelsWidget = channels , _threadsWidget = threads }

  fg `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire cli this key modifiers (keymapGlobal kb)

  threads `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire cli this key modifiers (keymapConvList kb)

  channels `onKeyPressed` \this key modifiers ->
    handleKey stateRef fire cli this key modifiers (keymapChanList kb)

  channels `onSelectionChange` \e ->
    case e of
      SelectionOn _ (Just chan) _ -> fire $ Client.SetChannel chan
      _                           -> return ()

  return cli

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

  when (prevState^.to channelGroups /= state^.to channelGroups) $
    renderChannels (_channelsWidget cli) (state^.to channelGroups)
  -- case currentChannelIndex state of
  --   Just n  -> setSelected (_channelsWidget cli) (n + 2)
  --   Nothing -> return ()

  when (prevState^.to conversations /= state^.to conversations) $
    renderThreads  (_threadsWidget  cli) (state^.to conversations)

  case state^.activity of
    Shutdown -> shutdownUi >> exitSuccess
    _ -> return ()

handleKey :: IORef State
          -> Handler Client.Event
          -> Cli
          -> Widget a
          -> Key -> [Modifier]
          -> Keymap
          -> IO Bool
handleKey stateRef fire cli _ key mods keymap = do
  upstreamState <- readIORef stateRef
  case Map.lookup (key, mods) keymap of
    Just act -> act fire cli upstreamState >> return True
    Nothing  -> return False

renderChannels :: Widget Channels -> [ChannelGroup] -> IO ()
renderChannels channels groups = do
  widgetGroups <- mapM widgetsForGroup groups
  emptyLabel   <- plainText ""
  let widgets = intercalate [(Nothing, emptyLabel)] widgetGroups
  clearList channels
  mapM_ (uncurry (addToList channels)) widgets

widgetsForGroup :: ChannelGroup -> IO [(Maybe Channel, Widget FormattedText)]
widgetsForGroup group = do
  widgets <- mapM plainText (map channelDisplay (group^.groupChannels))
  return $ zip (Just <$> group^.groupChannels) widgets

channelDisplay :: Channel -> Text
channelDisplay EmptyChannel    = ""
channelDisplay FlaggedChannel  = "Flagged"
channelDisplay NoListChannel   = "Direct"
channelDisplay (ListChannel l) = _mlId l

renderThreads :: Widget Threads -> [Conversation] -> IO ()
renderThreads threads ts = do
  clearList threads
  mapM_ (renderThread threads) ts

renderThread :: Widget Threads -> Conversation -> IO ()
renderThread threads conv = do
  -- let wlikes = if likes > 0 then summary `append` likeCount else summary
  -- let wlikes' = if liked then wlikes `append` " ↑" else wlikes
  -- t <- plainText wlikes'
  t <- plainText (fromMaybe "" (conv^.subject))
  addToList threads (conv^.convId) t
  -- where
  --   likeCount = pack ("\n+" ++ show likes)

showWelcome :: Widget Threads -> IO ()
showWelcome threads = do
  t <- plainText "Welcome to your email.  Use Ctrl+P and Ctrl+N to change channels."
  addToList threads "" t
