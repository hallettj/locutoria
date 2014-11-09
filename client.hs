{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (newAddHandler)

import Network.Mail.SocialMail.Cli (stepUi, ui)
import Network.Mail.SocialMail.Client (ClientConfig(..), ClientEvent(..), locutoria, stepData)
import Network.Mail.SocialMail.Notmuch (Database(..), DatabaseMode(..))

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
