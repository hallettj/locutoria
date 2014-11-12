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

instance Default Index where
  def = Index
    { iActivities = Map.empty
    , iChannels   = []
    , iLikeCounts = Map.empty
    , iThreads    = Map.empty
    }

fetchChannels :: Query -> IO (Index -> Index)
fetchChannels q = do
  addrs <- getListAddrs q
  return $ \index -> index { iChannels = addrs }

fetchThreads :: Database -> [ChannelId] -> IO (Index -> Index)
fetchThreads db chans = do
  ts <- mapM (getThreads db) chans
  let tmap = Map.fromList (zip chans ts)
  return $ \index -> index { iThreads = tmap }

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
toThread (tId, _, _, _) = Thread tId
