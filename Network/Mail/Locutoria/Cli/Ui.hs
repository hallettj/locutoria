{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Concurrent (Chan, forkIO, newChan, writeChan)
import           Control.Event.Handler (Handler)
import           Control.Lens
import           Data.Default (def)
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Text as Text
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
  stRef <- newIORef $ initialSt upstreamState
  chan  <- newChan
  let theApp = def { appDraw         = drawUi
                   , appChooseCursor = showFirstCursor
                   , appHandleEvent  = trackSt stRef $ uiEvent run kb fire
                   }
      update state = writeChan chan (ClientState state)
      run          = resumeUi chan theApp stRef
  return $ Client.Ui run update
  where
    trackSt :: IORef St -> (Event -> St -> IO St) -> Event -> St -> IO St
    trackSt ref f e st = do
      st' <- f e st
      writeIORef ref st'
      return st'

resumeUi :: Chan Event -> App St Event -> IORef St -> IO ()
resumeUi chan theApp stRef = do
  withVty (Vty.mkVty def) $ \vty -> do
    _      <- forkIO $ supplyVtyEvents vty VtyEvent chan
    (w, h) <- Vty.displayBounds $ Vty.outputIface vty
    st     <- readIORef stRef
    runVty vty chan theApp (st & stScreenSize .~ (w, h)
                               & stVty        .~ Just vty
                               & stNextAction .~ return ())
    st' <- readIORef stRef
    st'^.stNextAction

drawUi :: St -> [Render]
drawUi st = case st^.stUpstreamState.route of
  Root                 -> channelView st
  ShowChannel _ _      -> channelView st
  ShowConversation _ _ -> conversationView st
  ComposeReply _ _     -> undefined

uiEvent :: IO () -> KeyBindings -> Handler Client.Event -> Event -> St -> IO St
uiEvent continue kb fire e st = case e of
  VtyEvent (Vty.EvKey key mods) -> handleKey kb fire key mods st
  VtyEvent (Vty.EvResize w h)   -> return $ st & stScreenSize .~ (w, h)
  VtyEvent _                    -> return st
  ClientState state             ->
    case state^.route of
      ComposeReply chan conv -> compose continue fire chan conv (st & stUpstreamState .~ state)
      _                      ->
        let chans = flattenChannelGroups (channelGroups state)
        in return $ st & stChannels      %~ listReplace chans
                       & stConversations %~ listReplace (conversations state)
                       & stMessages      %~ listReplace (messages state)
                       & stUpstreamState .~ state

flattenChannelGroups :: [ChannelGroup] -> [Maybe Channel]
flattenChannelGroups groups =
  let chanLists  = map (map Just . (^.groupChannels)) groups
  in  intercalate [Nothing] chanLists

compose :: IO () -> Handler Client.Event -> Channel -> Conversation -> St -> IO St
compose continue fire chan conv st = do
  maybe (return ()) Vty.shutdown (st^.stVty)
  return $ st & stNextAction            .~ composeAction continue fire conv
              & stUpstreamState . route .~ ShowConversation chan conv

composeAction :: IO () -> Handler Client.Event -> Conversation -> IO ()
composeAction continue fire conv = do
  result <- C.composeReply (Text.pack (conv^.convId))
  case result of
    Right mail -> C.send mail
    Left  err  -> fire $ Client.GenericError (Text.pack (show err))
  continue
