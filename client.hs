{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (Handler)
import Data.Default (def)

import qualified Network.Mail.Locutoria.Cli as Cli
import Network.Mail.Locutoria.Client ( Config(..)
                                     , Event(..)
                                     , Ui(..)
                                     , locutoria
                                     )
import Network.Mail.Locutoria.Keymap (KeyBindings)
import Network.Mail.Locutoria.Notmuch ( Database(..)
                                      , DatabaseMode(..)
                                      )
import Network.Mail.Locutoria.State (State)

config :: IO Config
config = return $ Config db

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

keybindings :: KeyBindings
keybindings = def

ui :: Handler Event -> State -> IO Ui
ui = Cli.ui keybindings

main :: IO ()
main = do
  c <- config
  locutoria c ui
