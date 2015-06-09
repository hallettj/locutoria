{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import           Control.Event.Handler (Handler)
import           Control.Lens
import           Data.Default (def)
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
  chan  <- newChan
  let theApp = def { appDraw         = drawUi
                   , appChooseCursor = showFirstCursor
                   , appHandleEvent  = uiEvent run kb fire
                   }
      update state = writeChan chan (ClientState state)
      run          = resumeUi chan theApp def
  return $ Client.Ui (run (initialSt upstreamState)) update

resumeUi :: Chan Event -> App St Event -> RenderState -> St -> IO ()
resumeUi chan theApp rs st = do
  withVty (Vty.mkVty def) $ \vty -> do
    _      <- forkIO $ supplyVtyEvents vty VtyEvent chan
    (w, h) <- Vty.displayBounds $ Vty.outputIface vty
    runUi vty chan theApp rs (st & stScreenSize .~ (w, h))

runUi :: Vty.Vty -> Chan Event -> App St Event -> RenderState -> St -> IO ()
runUi vty chan app rs st = do
  newRs <- renderApp vty app st rs
  e <- readChan chan
  newSt <- appHandleEvent app e st
  case newSt^.stNextAction of
    Just act -> act (newSt & stNextAction .~ Nothing)
    Nothing  -> runUi vty chan app newRs newSt

drawUi :: St -> [Render]
drawUi st = case st^.stUpstreamState.route of
  Root                 -> channelView st
  ShowChannel _ _      -> channelView st
  ShowConversation _ _ -> conversationView st
  ComposeReply _ _     -> undefined

uiEvent :: (St -> IO ()) -> KeyBindings -> Handler Client.Event -> Event -> St -> IO St
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

compose :: (St -> IO ()) -> Handler Client.Event -> Channel -> Conversation -> St -> IO St
compose continue fire chan conv st = do
  return $ st & stNextAction            .~ Just (composeAction continue fire conv)
              & stUpstreamState . route .~ ShowConversation chan conv

composeAction :: (St -> IO ()) -> Handler Client.Event -> Conversation -> St -> IO ()
composeAction continue fire conv st = do
  result <- C.composeReply (Text.pack (conv^.convId))
  case result of
    Right mail -> C.send mail
    Left  err  -> fire $ Client.GenericError (Text.pack (show err))
  continue st
