{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Index where

import           Codec.ActivityStream.Dynamic
import           Control.Applicative ((<$>))
import           Control.Lens ((^.), makeLenses)
import           Data.Default (Default, def)
import           Data.List (nub, sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import Network.Mail.Locutoria.Conversation
import Network.Mail.Locutoria.MailingList
import Network.Mail.Locutoria.Message
import Network.Mail.Locutoria.Notmuch hiding (MessageId)

data Index = Index
  { _activities    :: Activities
  , _conversations :: [Conversation]
  -- , _lists      :: [MailingList]
  -- , _likeCounts :: LikeCounts
  -- , _threads    :: Threads
  }
  deriving (Eq, Show)

type Activities = Map MessageId [Activity]
-- type LikeCounts = Map MessageId Int
-- type Threads    = Map ChannelId [ThreadInfo]


instance Default Index where
  def = Index
    { _activities    = Map.empty
    , _conversations = []
    }


makeLenses ''Index

fetchRecentConversations :: Database -> IO (Index -> Index)
fetchRecentConversations db = do
  q  <- queryCreate db "date:1month.."
  ts <- queryThreads q
  cs <- mapM toConv ts
  return $ \index -> index { _conversations = cs }

lists :: Index -> [MailingList]
lists idx = nub $ sort $ catMaybes $ _convList <$> idx^.conversations

-- fetchLikeCounts :: Database -> Index -> IO (Index -> Index)
-- fetchLikeCounts _ index = do
--   let ts = join (Map.elems (_threads index))
--   likes <- join <$> mapM (getLikes . toThread) ts
--   let likes' = nub likes
--   let counts = foldl' incr (_likeCounts index) likes'
--   return $ \index' -> index' { _likeCounts = counts }
--   where
--     incr map (_, obj) = case mId obj of
--       Just m  -> Map.alter (\c -> Just (maybe 1 (+1) c)) m map
--       Nothing -> map
--     mId obj = if uriScheme obj == "mid:" then
--       Just (pack (takeWhile (/= '/') (uriPath obj)))
--     else
--       Nothing

-- likeCount :: Index -> MessageId -> Int
-- likeCount index mId = Map.findWithDefault 0 mId (_likeCounts index)

-- toThread :: ThreadInfo -> Thread
-- toThread (threadId, _, _, _) = Thread threadId
