{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui where

import           Control.Concurrent (Chan, forkIO, newChan, writeChan)
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

data UiBits = UiBits
  { _bitsChan :: Chan Event
  , _bitsApp  :: App St Event
  }

ui :: KeyBindings -> Handler Client.Event -> State -> IO Client.Ui
ui kb fire upstreamState = do
  chan     <- newChan
  let theApp = def { appDraw         = drawUi
                   , appChooseCursor = showFirstCursor
                   , appHandleEvent  = uiEvent (UiBits chan theApp) kb fire
                   }
  let update state = writeChan chan (ClientState state)
  let run          = resumeUi chan theApp upstreamState >> fire Client.Refresh
  return $ Client.Ui run update

resumeUi :: Chan Event -> App St Event -> State -> IO ()
resumeUi chan theApp state = do
  withVty (Vty.mkVty def) $ \vty -> do
    _ <- forkIO $ supplyVtyEvents vty VtyEvent chan
    (w, h) <- Vty.displayBounds $ Vty.outputIface vty
    runVty vty chan theApp (initialSt state vty & stScreenSize .~ (w, h))

drawUi :: St -> [Render St]
drawUi st = case st^.stUpstreamState.route of
  Root                 -> channelView st
  ShowChannel _ _      -> channelView st
  ShowConversation _ _ -> conversationView st
  ComposeReply _ _     -> undefined

uiEvent :: UiBits -> KeyBindings -> Handler Client.Event -> Event -> St -> IO St
uiEvent uiBits kb fire e st = case e of
  VtyEvent (Vty.EvKey key mods) -> handleKey kb fire key mods st
  VtyEvent (Vty.EvResize w h)   -> return $ st & stScreenSize .~ (w, h)
  VtyEvent _                    -> return st
  ClientState state             ->
    case state^.route of
      ComposeReply chan conv -> compose uiBits fire chan conv st
      _                   ->
        let chans = flattenChannelGroups (channelGroups state)
        in return $ st & stChannels      %~ listReplace chans
                       & stConversations %~ listReplace (conversations state)
                       & stMessages      %~ listReplace (messages state)
                       & stUpstreamState .~ state

flattenChannelGroups :: [ChannelGroup] -> [Maybe Channel]
flattenChannelGroups groups =
  let chanLists  = map (map Just . (^.groupChannels)) groups
  in  intercalate [Nothing] chanLists

compose :: UiBits -> Handler Client.Event -> Channel -> Conversation -> St -> IO St
compose (UiBits eventChan theApp) fire chan conv st = do
  Vty.shutdown $ st^.stVty
  result <- C.composeReply (Text.pack (conv^.convId))
  let newSt = st & stUpstreamState . route .~ ShowConversation chan conv
  resumeUi eventChan theApp (newSt^.stUpstreamState)
  case result of
    Right mail -> C.send mail >> fire (Client.SetRoute (ShowConversation chan conv))
    Left  err  -> fire $ Client.GenericError (Text.pack (show err))
  return newSt
