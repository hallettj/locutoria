{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Concurrent (Chan, forkIO, newChan, writeChan)
import           Control.Event.Handler (Handler)
import           Control.Lens
import           Data.Default (def)
import           Data.IORef
import           Data.List (intercalate)
import qualified Graphics.Vty as Vty

import           Brick.List
import           Brick.Main
import           Brick.Render

import           Network.Mail.Locutoria.Cli.Keymap
import           Network.Mail.Locutoria.Cli.Widgets
import qualified Network.Mail.Locutoria.Client as Client
import qualified Network.Mail.Locutoria.Compose as C
import           Network.Mail.Locutoria.Index hiding (_conversations, conversations)
import           Network.Mail.Locutoria.State

data Event = VtyEvent    Vty.Event
           | ClientState State
  deriving (Eq, Show)

ui :: KeyBindings -> Handler Client.Event -> State -> IO Client.Ui
ui kb fire upstreamState = do
  stateRef <- newIORef upstreamState
  chan     <- newChan
  let theApp = def { appDraw         = drawUi
                   , appChooseCursor = showFirstCursor
                   , appHandleEvent  = uiEvent kb fire
                   }
  let update state = writeIORef stateRef state >> writeChan chan (ClientState state)
  let run          = resumeUi chan theApp stateRef >> fire Client.Refresh
  return $ Client.Ui run update

resumeUi :: Chan Event -> App St Event -> IORef State -> IO ()
resumeUi chan theApp stateRef = do
  state <- readIORef stateRef
  withVty (Vty.mkVty def) $ \vty -> do
    _ <- forkIO $ supplyVtyEvents vty VtyEvent chan
    (w, h) <- Vty.displayBounds $ Vty.outputIface vty
    runVty vty chan theApp (initialSt state & stScreenSize .~ (w, h))

drawUi :: St -> [Render St]
drawUi st = case st^.stUpstreamState^.route of
  Root                 -> channelView st
  ShowChannel _ _      -> channelView st
  ShowConversation _ _ -> conversationView st
  ComposeReply _ _     -> undefined

uiEvent :: KeyBindings -> Handler Client.Event -> Event -> St -> IO St
uiEvent kb fire e st = case e of
  VtyEvent (Vty.EvKey key mods) -> handleKey kb fire key mods st
  VtyEvent (Vty.EvResize w h)   -> return $ st & stScreenSize .~ (w, h)
  VtyEvent _                    -> return st
  ClientState state             ->
    let chans = flattenChannelGroups (channelGroups state)
    in return $ st & stChannels      %~ listReplace chans
                   & stConversations %~ listReplace (conversations state)
                   & stMessages      %~ listReplace (messages state)
                   & stUpstreamState .~ state

flattenChannelGroups :: [ChannelGroup] -> [Maybe Channel]
flattenChannelGroups groups =
  let chanLists  = map (map Just . (^.groupChannels)) groups
  in  intercalate [Nothing] chanLists
