{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (Handler)
import Data.Default (def)

import           Network.Mail.Locutoria.Cli.Keymap (KeyBindings)
import qualified Network.Mail.Locutoria.Cli.Ui as Cli
import Network.Mail.Locutoria.Client ( Config(..)
                                     , Event(..)
                                     , Ui(..)
                                     , locutoria
                                     )
import Network.Mail.Locutoria.Notmuch ( Database(..)
                                      , DatabaseMode(..)
                                      )
import Network.Mail.Locutoria.State (State)
import Network.Mail.Mime (Address(..))

config :: IO Config
config = return $ Config
  { clDb = db
  , clUserAddr = me
  }

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

me :: Address
me = Address (Just "Jesse Hallett") "jesse@galois.com"

keybindings :: KeyBindings
keybindings = def

ui :: Config -> Handler Event -> State -> IO Ui
ui cfg = Cli.ui cfg keybindings

main :: IO ()
main = do
  c <- config
  locutoria c (ui c)
