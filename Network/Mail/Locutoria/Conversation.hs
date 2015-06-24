{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Conversation where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)

import Network.Mail.Locutoria.MailingList
import Network.Mail.Locutoria.Message
import Network.Mail.Locutoria.Notmuch hiding (MessageId)

data Conversation = Conversation
  { _convId       :: ThreadId
  , _convList     :: Maybe MailingList
  , _convMessages :: [Message]
  , _convSubject  :: Maybe Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''Conversation

toConv :: Thread -> IO Conversation
toConv t = do
  ms   <- threadGetMessagesFlat True t
  chan <- getMailingLists (_msgFilename <$> ms)
  return $ Conversation
    { _convId       = tId t
    , _convList     = listToMaybe chan
    , _convMessages = ms
    , _convSubject  = mHead ms >>= msgSubject
    }

tagged :: Text -> Conversation -> Bool
tagged tag c = any (\m -> tag `elem` _msgTags m) (c^.convMessages)

mHead :: [a] -> Maybe a
mHead [] = Nothing
mHead (x:_) = Just x
