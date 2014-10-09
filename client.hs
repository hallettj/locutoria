module Main where

import Data.DateTime (getCurrentTime)
import Data.Text (Text, pack)
import Network.Mail.Mime (Address(..), Mail(..), emptyMail, renderSendMailCustom)
import Network.Mail.SocialMail.Compose

main :: IO ()
main = do
  putStrLn "from: "
  sender <- getLine
  putStrLn "to: "
  recip <- getLine
  putStrLn "message: "
  msg <- getLine
  now <- getCurrentTime
  let note = noteActivity (toAddress sender) now (pack msg)
  let mail = (emptyMail (toAddress sender)) { mailTo = [toAddress recip] }
  let mail' = addActivity note mail
  renderSendMailCustom "/usr/bin/msmtp" [recip] mail'
