{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Cli where

import Control.Monad (forM_, mapM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput (Key(..), Modifier(..))
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)

import Network.Mail.SocialMail.Internal

ui :: [Text] -> IO ()
ui channels = do
  channels <- newTextList selectedItem channels 1
  messages <- (newList selectedItem 2 :: IO (Widget (List Text Table)))

  layout <- hBox channels messages
  setBoxChildSizePolicy layout (PerChild (BoxFixed 40) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  addToCollection c layout fg

  layout `onKeyPressed` \this key modifiers ->
    if key == KASCII 'q' then
      exitSuccess
    else
      return False

  layout `onKeyPressed` (channelControls channels)

  runUi c defaultContext

selectedItem :: Attr
selectedItem = Attr (SetTo standout) (SetTo bright_cyan) (SetTo black)

-- refreshChannels :: Widget Table -> IO ()
-- refreshChannels channels = do
--   addrs <- getListAddrs
--   cs <- mapM mkChannel addrs
--   forM_ (addRow channels) addrs

channelControls :: Show b => Widget (List a b) -> Widget c -> Key -> [Modifier] -> IO Bool
channelControls channels this key mods =
  if key == KASCII '@' then
    return False
    -- refreshChannels channels
  else if key == KASCII 'p' && MCtrl `elem` mods then
    do
      sel <- getSelected channels
      let n = fmap fst sel
      setSelected channels ((fromMaybe 1 n) - 1)
      return True
  else if key == KASCII 'n' && MCtrl `elem` mods then
    do
      sel <- getSelected channels
      let n = fmap fst sel
      setSelected channels ((fromMaybe (-1) n) + 1)
      return True
  else
    return False
