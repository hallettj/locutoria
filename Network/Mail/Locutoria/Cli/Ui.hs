{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Cli.Ui
  ( ui
  ) where

import           Control.Concurrent (newChan, writeChan)
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Data.Default (def)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty

import           Brick.Main
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Core

import           Network.Mail.Locutoria.Cli.Keymap
import           Network.Mail.Locutoria.Cli.Widgets
import qualified Network.Mail.Locutoria.Compose as C
import           Network.Mail.Locutoria.Config
import           Network.Mail.Locutoria.Event (Event, Handler)
import qualified Network.Mail.Locutoria.Event as Event
import           Network.Mail.Locutoria.State
import           Network.Mail.Locutoria.View

data UiEvent = VtyEvent Vty.Event
             | ClientEvent Event

ui :: Config -> State -> IO State
ui cfg initState = do
  chan <- newChan
  let fire = writeChan chan . ClientEvent
  let app = App { appDraw         = drawUi
                , appChooseCursor = showFirstCursor
                , appHandleEvent  = uiEvent cfg fire
                , appAttrMap      = const theAttrMap
                , appMakeVtyEvent = VtyEvent
                }
  customMain (Vty.mkVty def) chan app initState

drawUi :: State -> [Widget]
drawUi st = map (withBorderStyle unicode) $ case st^.to stView of
  Root                   -> channelView st
  ComposeReply _ _       -> undefined  -- TODO: should be unreachable
  ShowChannel _ _        -> channelView st
  ShowConversation _ _ _ -> conversationView st
  ShowQueue              -> undefined  -- TODO
  Quit                   -> undefined  -- TODO: should be unreachable

uiEvent :: Config -> Handler Event -> UiEvent -> State -> EventM (Next State)
uiEvent cfg fire e st = case e of
  VtyEvent (Vty.EvKey key mods) -> do
    case handleKey (cfg^.cfgKeyBindings) key mods st of
      Just e' -> liftIO $ fire e'
      Nothing -> return ()
    continue st
  VtyEvent (Vty.EvResize w h)   -> continue $ st & stScreenSize .~ (w, h)
  VtyEvent _                    -> continue st
  ClientEvent e'                -> do
    let (sideEffect, st') = Event.handle e' st
    liftIO $ sideEffect fire  -- TODO: run in background thread?
    handleExternal cfg fire st'

handleExternal :: Config -> Handler Event -> State -> EventM (Next State)
handleExternal cfg fire st = case st^.to stView of
  ComposeReply _ conv -> suspendAndResume $ do
    result <- C.composeReply (cfg^.cfgUserAddr) conv
    case result of
      Right mail -> fire $ Event.enqueue mail
      Left  err  -> fire $ Event.genericError (Text.pack (show err))
    return $ popView st
  Quit -> halt st
  _ -> continue st
