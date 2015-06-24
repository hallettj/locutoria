{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default (def)

import           Network.Mail.Locutoria (locutoria)
import           Network.Mail.Locutoria.Config
import           Network.Mail.Locutoria.Cli.Keymap (KeyBindings)
import           Network.Mail.Locutoria.Notmuch ( Database(..)
                                                , DatabaseMode(..)
                                                )
import           Network.Mail.Mime (Address(..))

config :: Config
config = Config
  { _cfgDb          = db
  , _cfgKeyBindings = keybindings
  , _cfgUserAddr    = me
  , _cfgSendCmd     = ["msmtp", "--read-envelope-from", "--read-recipients"]
  }

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

me :: Address
me = Address (Just "Jesse Hallett") "jesse@sitr.us"

keybindings :: KeyBindings
keybindings = def

main :: IO ()
main = locutoria config
