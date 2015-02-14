{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Index where

import           Codec.ActivityStream.Dynamic
import           Control.Applicative ((<$>))
import           Control.Lens (makeLenses)
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

data Index = Index
  { _activities :: Activities
  , _lists      :: [MailingList]
  , _likeCounts :: LikeCounts
  , _threads    :: Threads
  }
  deriving Show

type Activities = Map MessageId [Activity]
type LikeCounts = Map MessageId Int
type Threads    = Map ChannelId [ThreadInfo]


instance Default Index where
  def = Index
    { _activities = Map.empty
    , _lists      = []
    , _likeCounts = Map.empty
    , _threads    = Map.empty
    }


makeLenses ''Index

fetchChannels :: Database -> IO (Index -> Index)
fetchChannels db = do
  ls <- getMailingLists db
  return $ \index -> index { _lists = ls }

fetchThreads :: Database -> [MailingList] -> IO (Index -> Index)
fetchThreads db ls = do
  ts <- mapM (fetchListThreads db) ls
  let tmap = Map.fromList (zip (map mlId ls) ts)
  return $ \index -> index { _threads = tmap }

fetchListThreads :: Database -> MailingList -> IO [ThreadInfo]
fetchListThreads db list = do
  let postUris  = filter (\u -> uriScheme u == "mailto:") (mlPost list)
  let postAddrs = map (pack . uriPath) postUris
  threadLists <- mapM (getThreads db) postAddrs
  return $ join threadLists

fetchLikeCounts :: Database -> Index -> IO (Index -> Index)
fetchLikeCounts _ index = do
  let ts = join (Map.elems (_threads index))
  likes <- join <$> mapM (getLikes . toThread) ts
  let likes' = nub likes
  let counts = foldl' incr (_likeCounts index) likes'
  return $ \index' -> index' { _likeCounts = counts }
  where
    incr map (_, obj) = case mId obj of
      Just m  -> Map.alter (\c -> Just (maybe 1 (+1) c)) m map
      Nothing -> map
    mId obj = if uriScheme obj == "mid:" then
      Just (pack (takeWhile (/= '/') (uriPath obj)))
    else
      Nothing

likeCount :: Index -> MessageId -> Int
likeCount index mId = Map.findWithDefault 0 mId (_likeCounts index)

toThread :: ThreadInfo -> Thread
toThread (threadId, _, _, _) = Thread threadId
