{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Mail.SocialMail.Cli

import Network.Mail.SocialMail.Internal
import Network.Mail.SocialMail.Notmuch

main :: IO ()
main = do
  let db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly
  ui db
