{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Internal where

import Data.DateTime (getCurrentTime)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Text.ICU (MatchOption(..), Regex, find, group, regex)
-- import Foreign.Notmuch
import Network.Mail.SocialMail.Notmuch
import Network.Mail.Mime (Address(..), Mail(..), emptyMail, renderSendMailCustom)
import Network.Mail.SocialMail.Compose

type ChannelId = Text
type ThreadInfo = (ThreadId, Text, Int, Bool)

fromList :: Database -> IO Query
fromList db = queryCreate db "to:lists.galois.com"

getListAddrs :: Database -> IO [Text]
getListAddrs db = do
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

getThreads :: Database -> Text -> IO [ThreadInfo]
getThreads db chan = do
  q <- queryCreate db ("to:" ++ (unpack chan))
  -- queryThreads query
  r <- notmuch ["search", qText q]
  return $ map threadInfo (lines r)

threadInfo :: String -> ThreadInfo
threadInfo l = (tid, summary, 0, False)
  where
    (tid, rest) = splitAt 23 l
    summary = pack (drop 1 rest)
