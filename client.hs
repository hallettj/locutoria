{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (newAddHandler)

import Network.Mail.Locutoria.Cli (stepUi, ui)
import Network.Mail.Locutoria.Client (ClientConfig(..), ClientEvent(..), locutoria, stepData)
import Network.Mail.Locutoria.Notmuch (Database(..), DatabaseMode(..))

config :: ClientConfig
config = ClientConfig db
  where
    db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

main :: IO ()
main = do
  (addEvent, fireEvent) <- newAddHandler
  stepUi <- ui config fireEvent
  let stepData' = stepData fireEvent
  locutoria config addEvent fireEvent stepUi stepData'
  fireEvent GetChannels
  fireEvent GetLikeCounts
