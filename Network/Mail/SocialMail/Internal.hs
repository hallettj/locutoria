{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Internal where

import Data.DateTime (getCurrentTime)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Text.ICU (MatchOption(..), Regex, find, group, regex)
-- import Foreign.Notmuch
import Network.Mail.SocialMail.Notmuch
import Network.Mail.Mime (Address(..), Mail(..), emptyMail, renderSendMailCustom)
import Network.Mail.SocialMail.Compose

fromList :: Database -> IO Query
fromList db = queryCreate db "from:lists.galois.com OR to lists.galois.com"

getListAddrs :: IO [Text]
getListAddrs = do
  db <- databaseOpen "/home/jesse/mail/galois" DatabaseModeReadOnly
  query <- fromList db
  ts <- queryThreads query
  ls <- fmap concat (mapM threadGetRecipients ts)
  return $ nub (catMaybes (map parseListAddr ls))

parseListAddr :: Text -> Maybe Text
parseListAddr l = find listAddrExp l >>= group 0

listAddrExp :: Regex
listAddrExp = regex [CaseInsensitive] "[a-z]+@lists.galois.com"

compose :: IO ()
compose = do
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

toAddress :: String -> Address
toAddress email = Address { addressName = Nothing, addressEmail = pack email }

