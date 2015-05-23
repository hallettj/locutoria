{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Message where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens (makeLenses)
import           Data.Aeson ( FromJSON(..)
                            , (.:)
                            )
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import           Data.List (intersperse)
import           Data.Maybe (catMaybes)
import           Data.Monoid (mconcat)
import           Data.Text (Text, unpack)
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Network.URI (URI(..))

import           Network.Mail.Locutoria.Identifiable

type MessageId = Text

data MsgThread = MsgThread
  { _mtMsg :: Message
  , _mtReplies :: [MsgThread]
  }
  deriving Show

data Message = Message
  { _msgId :: MessageId
  , _msgHeaders :: [(Text, Text)]
  , _msgBody :: [MessagePart]
  , _msgFilename :: FilePath
  , _msgTags :: [Text]
  }
  deriving (Eq, Ord, Show)

data MessagePart = MessagePart
  { _mpId :: Int
  , _mpContentType :: Text
  , _mpContent :: Maybe Text
  , _mpSubparts :: [MessagePart]
  }
  deriving (Eq, Ord, Show)

makeLenses ''MsgThread
makeLenses ''Message
makeLenses ''MessagePart

instance FromJSON MsgThread where
  parseJSON (Ae.Array a) = if V.length a == 2 then
    MsgThread <$> msg <*> replies
  else
    fail "error parsing MsgThread: expected pair of message and reply list"
    where
      msg = parseJSON (a ! 0)
      replies = parseJSON (a ! 1)
  parseJSON _ = fail "error parsing MsgThread: expected array"

instance FromJSON Message where
  parseJSON (Ae.Object o) =
    Message <$> o .: "id"
            <*> (HM.toList <$> o .: "headers")
            <*> o .: "body"
            <*> o .: "filename"
            <*> o .: "tags"
    where
  parseJSON _ = fail "error parsing Message: expected object"

instance FromJSON MessagePart where
  parseJSON (Ae.Object o) =
    MessagePart <$> o .: "id"
                <*> o .: "content-type"
                <*> o .: "content"
                <*> subparts
    where
      content = HM.lookup "content" o
      subparts = case content of
        Just ps@(Ae.Array _) -> parseJSON ps
        Just _               -> pure []
        Nothing              -> pure []

  parseJSON _ = fail "error parsing MessagePart: expected object"

-- per RFC 2392
instance Identifiable Message where
  toUri m = URI
    { uriScheme = "mid:"
    , uriAuthority = Nothing
    , uriPath = unpack (_msgId m)
    , uriQuery = ""
    , uriFragment = ""
    }

-- per RFC 2392
instance Identifiable (Message, MessagePart) where
  toUri (m, p) = URI
    { uriScheme = "mid:"
    , uriAuthority = Nothing
    , uriPath = unpack (_msgId m) ++ "/" ++ show (_mpId p)
    , uriQuery = ""
    , uriFragment = ""
    }

messagesHeaderValues :: Text -> [Message] -> [Text]
messagesHeaderValues h ms = catMaybes $ map (lookup h . _msgHeaders) ms

messageHeaderValue :: Text -> Message -> Maybe Text
messageHeaderValue h = lookup h . _msgHeaders

-- TODO: use a Foldable instance
flatThread :: [MsgThread] -> [Message]
flatThread = (>>= tMsgs)
  where
    tMsgs t = _mtMsg t : flatThread (_mtReplies t)

msgText :: Message -> Text
msgText = mconcat . intersperse "\n" . catMaybes . map _mpContent . filter plainText . flatParts
  where
    plainText = (== "text/plain") . _mpContentType
    flatParts m = (_msgBody m) >>= _mpSubparts  -- TODO: this only flattens two levels
