{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Concurrent (Chan, forkIO, newChan, writeChan)
import           Control.Event.Handler (Handler)
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Data.Default (def)
import           Data.List (intercalate)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty

import           Brick.Border.Style
import           Brick.List
import           Brick.Main
import           Brick.Render

import           Network.Mail.Locutoria.Cli.Keymap
import           Network.Mail.Locutoria.Cli.Widgets
import qualified Network.Mail.Locutoria.Client as Client
import qualified Network.Mail.Locutoria.Compose as C
import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.State

data Event = VtyEvent    Vty.Event
           | ClientState State
  deriving (Eq, Show)

ui :: Client.Config -> KeyBindings -> Handler Client.Event -> State -> IO Client.Ui
ui cfg kb fire upstreamState = do
  chan  <- newChan
  let theApp = def { appDraw         = drawUi
                   , appChooseCursor = showFirstCursor
                   , appHandleEvent  = uiEvent run cfg kb fire
                   , appAttrMap      = const theAttrMap
                   }
      update state = writeChan chan (ClientState state)
      run          = resumeUi chan theApp def
  return $ Client.Ui (run (initialSt upstreamState)) update

resumeUi :: Chan Event -> App St Event -> RenderState -> St -> IO ()
resumeUi chan theApp rs st = do
  withVty (Vty.mkVty def) $ \vty -> do
    _      <- forkIO $ supplyVtyEvents vty VtyEvent chan
    (w, h) <- Vty.displayBounds $ Vty.outputIface vty
    runUi vty chan theApp (st & stScreenSize .~ (w, h)) rs

runUi :: Vty.Vty -> Chan Event -> App St Event -> St -> RenderState -> IO ()
runUi vty chan app st rs = do
  (newSt, newRs) <- stepVty vty chan app st rs
  case newSt^.stNextAction of
    Just act -> Vty.shutdown vty >> act (newSt & stNextAction .~ Nothing)
    Nothing  -> runUi vty chan app newSt newRs

drawUi :: St -> [Render]
drawUi st = map (withBorderStyle unicode) $ case st^.stUpstreamState.route of
  Root                 -> channelView st
  ShowChannel _ _      -> channelView st
  ShowConversation _ _ -> conversationView st
  ComposeReply _ _     -> undefined

uiEvent :: (St -> IO ()) -> Client.Config -> KeyBindings -> Handler Client.Event -> Event -> St -> EventM St
uiEvent continue cfg kb fire e st = case e of
  VtyEvent (Vty.EvKey key mods) -> liftIO $ handleKey kb fire key mods st
  VtyEvent (Vty.EvResize w h)   -> return $ st & stScreenSize .~ (w, h)
  VtyEvent _                    -> return st
  ClientState state             ->
    case state^.route of
      ComposeReply chan conv -> compose continue cfg fire chan conv (st & stUpstreamState .~ state)
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

compose :: (St -> IO ()) -> Client.Config -> Handler Client.Event -> Channel -> Conversation -> St -> EventM St
compose continue cfg fire chan conv st = do
  return $ st & stNextAction            .~ Just (composeAction continue cfg fire conv)
              & stUpstreamState . route .~ ShowConversation chan conv

composeAction :: (St -> IO ()) -> Client.Config -> Handler Client.Event -> Conversation -> St -> IO ()
composeAction continue cfg fire conv st = do
  result <- C.composeReply (Client.clUserAddr cfg) conv
  case result of
    Right mail -> C.send (Client.clSendCmd cfg) mail
    Left  err  -> fire $ Client.GenericError (Text.pack (show err))
  continue st
