{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Index where

import           Codec.ActivityStream.Dynamic
import           Control.Applicative ((<$>))
import           Control.Monad (join)
import           Data.List (foldl', nub, takeWhile)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack)
import           Network.URI (URI(..))

import Network.Mail.Locutoria.Internal
import Network.Mail.Locutoria.Notmuch

type Activities = Map MessageId [Activity]
type LikeCounts = Map MessageId Int
type Threads    = Map ChannelId [ThreadInfo]

data Index = Index
  { iActivities :: Activities
  , iChannels   :: [ChannelId]
  , iLikeCounts :: LikeCounts
  , iThreads    :: Threads
  }

empty :: Index
empty = Index
  { iActivities = Map.empty
  , iChannels   = []
  , iLikeCounts = Map.empty
  , iThreads    = Map.empty
  }

fetchChannels :: Database -> Index -> IO (Index -> Index)
fetchChannels db index = do
  addrs <- getListAddrs db
  return $ \index' -> index' { iChannels = addrs }

fetchThreads :: Database -> Index -> [ChannelId] -> IO (Index -> Index)
fetchThreads db index chans = do
  ts <- mapM (getThreads db) chans
  let tmap = Map.fromList (zip chans ts)
  return $ \index' -> index' { iThreads = tmap }

fetchLikeCounts :: Database -> Index -> IO (Index -> Index)
fetchLikeCounts db index = do
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
toThread (tId, _, _, _) = Thread tId
