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
import Network.Mail.Locutoria.Notmuch ( Database(..)
                                      , DatabaseMode(..)
                                      , Query
                                      , queryCreate
                                      )

config :: IO ClientConfig
config = do
  q <- listTraffic
  return $ ClientConfig db q

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

listTraffic :: IO Query
listTraffic = queryCreate db "to:lists.galois.com"

main :: IO ()
main = do
  putStrLn ("Indexing mail in " ++ dLoc db ++ " ...")
  c        <- config
  channels <- fetchChannels (clQuery c)
  let state = def { clIndex = channels def }
  (addEvent, fireEvent) <- newAddHandler
  stepUi <- ui fireEvent
  let stepData' = stepData fireEvent
  locutoria c state addEvent stepUi stepData'
  fireEvent GetLikeCounts
