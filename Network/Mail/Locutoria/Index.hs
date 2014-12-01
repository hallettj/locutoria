{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Index where

import           Codec.ActivityStream.Dynamic
import           Control.Applicative ((<$>))
import           Control.Monad (join)
import           Data.Default (Default, def)
import           Data.List (foldl', nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (pack)
import           Network.URI (URI(..))

import Network.Mail.Locutoria.Internal
import Network.Mail.Locutoria.MailingList
import Network.Mail.Locutoria.Notmuch

type Activities = Map MessageId [Activity]
type LikeCounts = Map MessageId Int
type Threads    = Map ChannelId [ThreadInfo]

data Index = Index
  { iActivities :: Activities
  , iLists      :: [MailingList]
  , iLikeCounts :: LikeCounts
  , iThreads    :: Threads
  }

instance Default Index where
  def = Index
    { iActivities = Map.empty
    , iLists      = []
    , iLikeCounts = Map.empty
    , iThreads    = Map.empty
    }

fetchChannels :: Database -> IO (Index -> Index)
fetchChannels db = do
  lists <- getMailingLists db
  return $ \index -> index { iLists = lists }

fetchThreads :: Database -> [MailingList] -> IO (Index -> Index)
fetchThreads db lists = do
  ts <- mapM (fetchListThreads db) lists
  let tmap = Map.fromList (zip (map mlId lists) ts)
  return $ \index -> index { iThreads = tmap }

fetchListThreads :: Database -> MailingList -> IO [ThreadInfo]
fetchListThreads db list = do
  let postUris  = filter (\u -> uriScheme u == "mailto:") (mlPost list)
  let postAddrs = map (pack . uriPath) postUris
  threadLists <- mapM (getThreads db) postAddrs
  return $ join threadLists

fetchLikeCounts :: Database -> Index -> IO (Index -> Index)
fetchLikeCounts _ index = do
  let ts = join (Map.elems (iThreads index))
  likes <- join <$> mapM (getLikes . toThread) ts
  let likes' = nub likes
  let counts = foldl' incr (iLikeCounts index) likes'
  return $ \index' -> index' { iLikeCounts = counts }
  where
    incr map (_, obj) = case mId obj of
      Just m  -> Map.alter (\c -> Just (maybe 1 (+1) c)) m map
      Nothing -> map
    mId obj = if uriScheme obj == "mid:" then
      Just (pack (takeWhile (/= '/') (uriPath obj)))
    else
      Nothing

likeCount :: Index -> MessageId -> Int
likeCount index mId = Map.findWithDefault 0 mId (iLikeCounts index)

toThread :: ThreadInfo -> Thread
toThread (threadId, _, _, _) = Thread threadId
