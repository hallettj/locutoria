{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Event.Handler (newAddHandler)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)

import Network.Mail.Locutoria.Cli (ui)
import Network.Mail.Locutoria.Client ( ClientConfig(..)
                                     , ClientEvent(..)
                                     , locutoria
                                     , stepData
                                     )
import Network.Mail.Locutoria.Notmuch ( Database(..)
                                      , DatabaseMode(..)
                                      )

config :: IO ClientConfig
config = return $ ClientConfig db

db :: Database
db = Database "/home/jesse/mail/galois" DatabaseModeReadOnly

main :: IO ()
main = do
  c <- config
  (addEvent, fireEvent) <- newAddHandler
  (stepUi, runUi) <- ui fireEvent
  let stepData' = stepData fireEvent
  locutoria c addEvent stepUi stepData'
  _ <- installHandler sigINT (Catch (fireEvent ClientExit)) Nothing
  putStrLn ("Indexing mail in " ++ dLoc db ++ " ...")
  fireEvent Refresh
  runUi
