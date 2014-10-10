{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (newAddHandler)

import Network.Mail.SocialMail.Cli
import Network.Mail.SocialMail.Client
import Network.Mail.SocialMail.Internal
import Network.Mail.SocialMail.Notmuch

main :: IO ()
main = do
  let db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly
  let config = ClientConfig db
  ui config
