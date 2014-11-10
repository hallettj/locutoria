{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (newAddHandler)
import Data.Default (def)

import Network.Mail.Locutoria.Cli (ui)
import Network.Mail.Locutoria.Client ( ClientConfig(..)
                                     , ClientState(..)
                                     , ClientEvent(..)
                                     , locutoria
                                     , stepData
                                     )
import Network.Mail.Locutoria.Index (fetchChannels)
import Network.Mail.Locutoria.Notmuch (Database(..), DatabaseMode(..))

config :: ClientConfig
config = ClientConfig db

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

main :: IO ()
main = do
  putStrLn ("Indexing mail in " ++ dLoc db ++ " ...")
  channels <- fetchChannels db
  let state = def { clIndex = channels def }
  (addEvent, fireEvent) <- newAddHandler
  stepUi <- ui fireEvent
  let stepData' = stepData fireEvent
  locutoria config state addEvent stepUi stepData'
  fireEvent GetLikeCounts
