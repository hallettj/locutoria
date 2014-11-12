{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Internal where

import Codec.ActivityStream.Dynamic
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.))
import Control.Monad (join)
import Data.Aeson (decode)
import Data.DateTime (getCurrentTime)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Text.ICU (MatchOption(..), Regex, find, group, regex)
-- import Foreign.Notmuch
import Prelude hiding (recip)

import Network.URI (URI, parseURI)
import Network.Mail.Locutoria.Notmuch
import Network.Mail.Mime (Address(..), Mail(..), emptyMail, renderSendMailCustom)
import Network.Mail.Locutoria.Compose

type ChannelId = Text
type ThreadInfo = (ThreadId, Text, Int, Bool)

getListAddrs :: Query -> IO [Text]
getListAddrs query = do
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

getThreadActivities :: Thread -> IO [Activity]
getThreadActivities t = do
  ms <- flatThread <$> threadGetMessages t
  join <$> mapM getMessageActivities ms

getMessageActivities :: Message -> IO [Activity]
getMessageActivities m = do
  let parts    = messageParts m
  let actParts = filter actPart parts
  partsBS <- mapM getPart actParts
  return $ catMaybes (map decode partsBS)
  where
    actPart = (== "application/stream+json") . mpContentType  -- TODO: mimetype parser?
    getPart p = notmuchBS ["show", "--part=" ++ show (mpId p), "id:" ++ unpack (msgId m)]

getLikes :: Thread -> IO [(URI, URI)]
getLikes t = do
  acts <- getThreadActivities t
  let likeActs = filter ((== Just like) . (^. acVerb)) acts
  return $ catMaybes (map uriPair likeActs)
  where
    like = "like" :: Text
    objUri a = do
      obj    <- a ^. acObject
      objId  <- obj ^. oId
      parseURI (unpack objId)
    actorUri a = do
      let actor =  a ^. acActor
      actId  <- actor ^. oId
      parseURI (unpack actId)
    uriPair a = (,) <$> actorUri a <*> objUri a

messageParts :: Message -> [MessagePart]
messageParts m = msgBody m >>= parts
  where
    -- TODO: check content-type for "multipart/*"
    parts p = p : mpSubparts p

getThread :: Database -> MessageId -> IO Thread
getThread db mId = do
  q <- queryCreate db ("id:" ++ unpack mId)
  ts <- queryThreads q
  return $ head ts
