{-# LANGUAGE RankNTypes #-}

module Network.Mail.Locutoria.Cli.Keymap where

import           Control.Lens hiding (lens)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Default (Default, def)
import           Graphics.Vty.Input (Key(..), Modifier(..))

import           Network.Mail.Locutoria.Event
import           Network.Mail.Locutoria.State
import           Network.Mail.Locutoria.View

data KeyBindings = KeyBindings
  { _keymapGlobal           :: Keymap
  , _keymapChannelView      :: Keymap
  , _keymapConversationView :: Keymap
  }

type Keymap = Map KeyCombo Event
type KeyCombo = (Key, [Modifier])


handleKey :: KeyBindings -> Key -> [Modifier] -> State -> Maybe Event
handleKey kb key mods st = case st^.stView of
  Root                   -> tryKeymaps [_keymapGlobal]
  ComposeReply _ _       -> tryKeymaps [_keymapGlobal]
  ShowChannel _ _        -> tryKeymaps [_keymapChannelView, _keymapGlobal]
  ShowConversation _ _ _ -> tryKeymaps [_keymapConversationView, _keymapGlobal]
  ShowQueue              -> tryKeymaps [_keymapGlobal]
  Quit                   -> tryKeymaps [_keymapGlobal]
  where
    tryKeymaps []     = Nothing
    tryKeymaps (m:ms) = case Map.lookup (key, mods) (m kb) of
      Just e  -> Just e
      Nothing -> tryKeymaps ms

instance Default KeyBindings where
  def = KeyBindings defKeymapGlobal defKeymapChannelView defKeymapConversationView

defKeymapGlobal :: Map KeyCombo Event
defKeymapGlobal = Map.fromList
  [ ((KChar 'q', []),      quit)
  , ((KChar '@', []),      refresh)
  , ((KChar 'p', [MCtrl]), prevChannel)
  , ((KChar 'n', [MCtrl]), nextChannel)
  ]

defKeymapChannelView :: Map KeyCombo Event
defKeymapChannelView = Map.fromList
  [ ((KChar 'r', []), composeReply)
  , ((KEnter,    []), showConv)
  , ((KChar 'j', []), nextConv)
  , ((KChar 'k', []), prevConv)
  , ((KChar 'g', []), setConv 0)
  , ((KChar 'G', []), setConv (-1))
  ]

defKeymapConversationView :: Map KeyCombo Event
defKeymapConversationView = Map.fromList
  [ ((KChar 'r', []), composeReply)
  , ((KChar 'j', []), nextMsg)
  , ((KChar 'k', []), prevMsg)
  , ((KChar 'g', []), setMsg 0)
  , ((KChar 'G', []), setMsg (-1))
  ]
