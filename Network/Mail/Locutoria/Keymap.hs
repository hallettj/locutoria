module Network.Mail.Locutoria.Keymap where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (Handler)
import           Control.Lens ((^.))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Default (Default, def)
import           Graphics.Vty.Input (Key(..), Modifier(..))

import qualified Network.Mail.Locutoria.Client as Client
import           Network.Mail.Locutoria.Internal
import           Network.Mail.Locutoria.MailingList
import           Network.Mail.Locutoria.Notmuch
import           Network.Mail.Locutoria.State (State)
import qualified Network.Mail.Locutoria.State as State

data KeyBindings = KeyBindings
  { keymapGlobal   :: Keymap
  , keymapConvList :: Keymap
  , keymapChanList :: Keymap
  }

type Keymap = Map KeyCombo UiAction
type KeyCombo = (Key, [Modifier])

type UiAction = Handler Client.Event -> State -> IO ()

instance Default KeyBindings where
  def = KeyBindings defKeymapGlobal defKeymapConvList defKeymapChanlist

defKeymapGlobal :: Map KeyCombo UiAction
defKeymapGlobal = Map.fromList
  [ ((KChar 'q', []),      \fire _ -> fire Client.Quit)
  , ((KChar '@', []),      \fire _ -> fire Client.Refresh)
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
incChannel f fire s = fire $ Client.SetChannel (mlId <$> chan)
  where
    cs   = s^.State.channels
    idx  = wrap (fromMaybe 0 (f <$> (State.currentChannelIndex s))) (length cs)
    chan = if length cs > idx then Just (cs !! idx) else Nothing

incConv :: (Int -> Int) -> UiAction
incConv f fire s = fire $ Client.SetThread (threadId <$> conv)
  where
    cs   = s^.State.conversations
    idx  = wrap (fromMaybe 0 (f <$> (State.currentConversationIndex s))) (length cs)
    conv = if length cs > idx then Just (cs !! idx) else Nothing

wrap :: Int -> Int -> Int
wrap n len | len > 0   = if n < 0 then len - n else n `mod` len
           | otherwise = 0

threadId :: ThreadInfo -> ThreadId
threadId (tId', _, _, _) = tId'

composeReply :: UiAction
composeReply fire s = case s^.State.selectedThread of
  Just thread -> fire $ Client.ComposeReply thread
  Nothing     -> return ()
