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
import           Network.Mail.Locutoria.State

data KeyBindings = KeyBindings
  { _keymapGlobal   :: Keymap
  , _keymapConvList :: Keymap
  , _keymapChanList :: Keymap
  }

type Keymap = Map KeyCombo UiAction
type KeyCombo = (Key, [Modifier])

type UiAction = Handler Client.Event -> St -> IO St


handleKey :: KeyBindings -> Handler Client.Event -> Key -> [Modifier] -> St -> IO St
handleKey kb fire key mods st =
  tryKeymaps [_keymapConvList, _keymapGlobal]
  -- TODO: check keymaps based on current route
  where
    tryKeymaps []     = return st
    tryKeymaps (m:ms) = case Map.lookup (key, mods) (m kb) of
      Just action -> action fire st
      Nothing     -> tryKeymaps ms

instance Default KeyBindings where
  def = KeyBindings defKeymapGlobal defKeymapConvList defKeymapChanlist

defKeymapGlobal :: Map KeyCombo UiAction
defKeymapGlobal = Map.fromList
  [ ((KChar 'q', []),      \fire st -> fire Client.Quit >> return st)
  , ((KChar '@', []),      \fire st -> fire Client.Refresh >> return st)
  , ((KChar 'p', [MCtrl]), setChannel (moveBy (-1)))
  , ((KChar 'n', [MCtrl]), setChannel (moveBy 1))
  ]

defKeymapConvList :: Map KeyCombo UiAction
defKeymapConvList = Map.fromList
  [ ((KChar 'r', []), composeReply)
  , ((KChar 'j', []), setConv (moveBy 1))
  , ((KChar 'k', []), setConv (moveBy (-1)))
  , ((KChar 'g', []), setConv (moveTo 0))
  , ((KChar 'G', []), setConv (moveTo (-1)))
  ]

defKeymapChanlist :: Map KeyCombo UiAction
defKeymapChanlist = Map.empty

setChannel :: (List (Maybe Channel) -> List (Maybe Channel)) -> UiAction
setChannel = setItem stChannels $ const (Client.SetRoute . ShowChannel <$>)

setConv :: (List Conversation -> List Conversation) -> UiAction
setConv = setItem stConversations $ \state conv ->
  (\chan -> Client.SetRoute (ShowConversation chan conv)) <$> selectedChannel state

setItem :: Lens' St (List e) -> (State -> e -> Maybe Client.Event) -> (List e -> List e) -> UiAction
setItem lens mkEvent f fire st = do
  let st' = st & lens %~ f
  case listSelectedElement (st'^.lens) of
    Just (_, v) -> maybe (return ()) fire $ mkEvent (st^.stUpstreamState) v
    Nothing     -> return ()
  return st'

composeReply :: UiAction
composeReply fire st = do
  case (st^.stUpstreamState^.to selectedChannel,
        st^.stUpstreamState^.to selectedConversation) of
    (Just chan, Just conv) -> fire (Client.SetRoute $ ComposeReply chan conv)
    _                      -> return ()
  return st
