{-# LANGUAGE RankNTypes #-}

module Network.Mail.Locutoria.Cli.Keymap where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (Handler)
import           Control.Lens hiding (lens)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Default (Default, def)
import           Graphics.Vty.Input (Key(..), Modifier(..))

import           Brick.List (List, listSelectedElement, moveBy, moveTo)

import           Network.Mail.Locutoria.Cli.Widgets
import qualified Network.Mail.Locutoria.Client as Client
import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.Message
import           Network.Mail.Locutoria.State

data KeyBindings = KeyBindings
  { _keymapGlobal           :: Keymap
  , _keymapChannelView      :: Keymap
  , _keymapConversationView :: Keymap
  }

type Keymap = Map KeyCombo UiAction
type KeyCombo = (Key, [Modifier])

type UiAction = Handler Client.Event -> St -> IO St


handleKey :: KeyBindings -> Handler Client.Event -> Key -> [Modifier] -> St -> IO St
handleKey kb fire key mods st = case st^.stUpstreamState^.route of
  Root                 -> tryKeymaps [_keymapChannelView, _keymapGlobal]
  ShowChannel _ _      -> tryKeymaps [_keymapChannelView, _keymapGlobal]
  ShowConversation _ _ -> tryKeymaps [_keymapConversationView, _keymapGlobal]
  ComposeReply _ _     -> tryKeymaps [_keymapGlobal]
  where
    tryKeymaps []     = return st
    tryKeymaps (m:ms) = case Map.lookup (key, mods) (m kb) of
      Just action -> action fire st
      Nothing     -> tryKeymaps ms

instance Default KeyBindings where
  def = KeyBindings defKeymapGlobal defKeymapChannelView defKeymapConversationView

defKeymapGlobal :: Map KeyCombo UiAction
defKeymapGlobal = Map.fromList
  [ ((KChar 'q', []),      \fire st -> fire Client.Quit >> return st)
  , ((KChar '@', []),      \fire st -> fire Client.Refresh >> return st)
  , ((KChar 'p', [MCtrl]), setChannel (moveBy (-1)))
  , ((KChar 'n', [MCtrl]), setChannel (moveBy 1))
  ]

defKeymapChannelView :: Map KeyCombo UiAction
defKeymapChannelView = Map.fromList
  [ ((KChar 'r', []), composeReply)
  , ((KEnter,    []), showConv)
  , ((KChar 'j', []), setConv (moveBy 1))
  , ((KChar 'k', []), setConv (moveBy (-1)))
  , ((KChar 'g', []), setConv (moveTo 0))
  , ((KChar 'G', []), setConv (moveTo (-1)))
  ]

defKeymapConversationView :: Map KeyCombo UiAction
defKeymapConversationView = Map.fromList
  [ ((KChar 'r', []), composeReply)
  , ((KChar 'j', []), setMsg (moveBy 1))
  , ((KChar 'k', []), setMsg (moveBy (-1)))
  , ((KChar 'g', []), setMsg (moveTo 0))
  , ((KChar 'G', []), setMsg (moveTo (-1)))
  ]

setChannel :: (List (Maybe Channel) -> List (Maybe Channel)) -> UiAction
setChannel = setItem stChannels $ \_ chan -> Client.SetRoute . flip ShowChannel Nothing <$> chan

setConv :: (List Conversation -> List Conversation) -> UiAction
setConv = setItem stConversations $ \state conv ->
  (\chan -> Client.SetRoute (ShowChannel chan (Just conv))) <$> selectedChannel state

setMsg :: (List Message -> List Message) -> UiAction
setMsg = setItem stMessages (const (const Nothing))
-- TODO: reflect selected message in state

setItem :: Lens' St (List e) -> (State -> e -> Maybe Client.Event) -> (List e -> List e) -> UiAction
setItem lens mkEvent f fire st = do
  let st' = st & lens %~ f
  case listSelectedElement (st'^.lens) of
    Just (_, v) -> maybe (return ()) fire $ mkEvent (st^.stUpstreamState) v
    Nothing     -> return ()
  return st'

showConv :: UiAction
showConv fire st = do
  case (st^.stUpstreamState^.to selectedChannel,
        st^.stUpstreamState^.to selectedConversation) of
    (Just chan, Just conv) -> fire (Client.SetRoute $ ShowConversation chan conv)
    _                      -> return ()
  return st

composeReply :: UiAction
composeReply fire st = do
  case (st^.stUpstreamState^.to selectedChannel,
        st^.stUpstreamState^.to selectedConversation) of
    (Just chan, Just conv) -> fire (Client.SetRoute $ ComposeReply chan conv)
    _                      -> return ()
  return st
