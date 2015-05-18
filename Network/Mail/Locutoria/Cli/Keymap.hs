module Network.Mail.Locutoria.Cli.Keymap where

import           Control.Event.Handler (Handler)
import           Control.Lens ((^.))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Default (Default, def)
import           Graphics.Vty.Input (Key(..), Modifier(..))
import           Graphics.Vty.Widgets.All hiding (Handler)

import           Network.Mail.Locutoria.Cli
import qualified Network.Mail.Locutoria.Client as Client
import           Network.Mail.Locutoria.State (State)
import qualified Network.Mail.Locutoria.State as State

data KeyBindings = KeyBindings
  { keymapGlobal   :: Keymap
  , keymapConvList :: Keymap
  , keymapChanList :: Keymap
  }

type Keymap = Map KeyCombo UiAction
type KeyCombo = (Key, [Modifier])

type UiAction = Handler Client.Event -> Cli -> State -> IO ()

instance Default KeyBindings where
  def = KeyBindings defKeymapGlobal defKeymapConvList defKeymapChanlist

defKeymapGlobal :: Map KeyCombo UiAction
defKeymapGlobal = Map.fromList
  [ ((KChar 'q', []),      \fire _ _ -> fire Client.Quit)
  , ((KChar '@', []),      \fire _ _ -> fire Client.Refresh)
  , ((KChar 'p', [MCtrl]), incChannel (\n -> n - 1))
  , ((KChar 'n', [MCtrl]), incChannel (+1))
  ]

defKeymapConvList :: Map KeyCombo UiAction
defKeymapConvList = Map.fromList
  [ ((KChar 'r', []),     composeReply)
  , ((KChar 'j', []),     incConv (+1))
  , ((KChar 'k', []),     incConv (\n -> n - 1))
  , ((KChar 'g', []),     incConv (const 0))
  , ((KChar 'G', []),     incConv (const (-1)))
  ]

defKeymapChanlist :: Map KeyCombo UiAction
defKeymapChanlist = Map.empty

incChannel :: (Int -> Int) -> UiAction
incChannel f _ cli _ = incWidget f (cli^.channelsWidget)

incConv :: (Int -> Int) -> UiAction
incConv f _ cli _ = incWidget f (cli^.threadsWidget)

incWidget :: (Int -> Int) -> Widget (List a b) -> IO ()
incWidget f widget = do
  curr <- getSelected widget
  len  <- getListSize widget
  let idx     = maybe 0 (f . fst) curr
  let nextIdx = if idx < len then idx else 0
  if len > 0 then
    setSelected widget nextIdx
  else
    return ()

composeReply :: UiAction
composeReply fire _ s = case s^.State.selectedThread of
  Just thread -> fire $ Client.ComposeReply thread
  Nothing     -> return ()
