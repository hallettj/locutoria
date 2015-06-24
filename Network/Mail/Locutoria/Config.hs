{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Config where

import           Control.Lens
import           Network.Mail.Mime (Address)

import           Network.Mail.Locutoria.Cli.Keymap (KeyBindings)
import           Network.Mail.Locutoria.Notmuch (Database)

data Config = Config
  { _cfgDb          :: Database
  , _cfgKeyBindings :: KeyBindings
  , _cfgUserAddr    :: Address
  , _cfgSendCmd     :: [String]
  }

makeLenses ''Config
