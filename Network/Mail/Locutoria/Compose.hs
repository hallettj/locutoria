{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mail.Locutoria.Compose where

import           Control.Applicative ((<$>))
import           Control.Lens ((&), (.~), (^.))
import           Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LB
import           Data.DateTime (DateTime)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (fromChunks)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Codec.ActivityStream.Dynamic
import           Control.Monad (join)
import           Network.Mail.Mime ( Address(..)
                                   , Encoding(..)
                                   , Mail(..)
                                   , Part(..)
                                   , addPart, renderMail', sendmailCustom)
import           Network.Mail.Mime.Parser (ParseError, parseMessage)
import           Network.URI (URI(..), parseURI)
import           System.IO (hClose)
import           System.Posix.Env (getEnvDefault)
import           System.Posix.Files (removeLink)
import           System.Posix.IO
import qualified System.Posix.Temp as Temp
import           System.Process

import Network.Mail.Locutoria.Identifiable
import Network.Mail.Locutoria.Notmuch

data MessageParams = MessageParams
  { msgFrom :: Maybe Address
  , msgTo :: Maybe Address
  , msgSubject :: Maybe Text
  }
  deriving Show

-- send :: Mail -> IO ()
-- send mail = renderMail' mail >>= (\bs -> sendmailCustom "/usr/bin/tee" ["/home/jesse/sent"] bs)

send :: Mail -> IO ()
send mail = renderMail' mail >>= LB.writeFile "/home/jesse/sent"

-- send :: Mail -> IO ()
-- send mail = renderMail' mail >>= (\bs -> sendmailCustom "/usr/bin/env"
--   [ "msmtp"
--   , "--read-envelope-from"
--   , "--read-recipients"] bs)

composeReply :: SearchTerm -> IO (Either ParseError Mail)
composeReply term = do
  template <- notmuchBS ["reply", Text.unpack term]
  output   <- inputViaEditor template
  return $ parseMessage "message" output

inputViaEditor :: LB.ByteString -> IO LB.ByteString
inputViaEditor input = do
  (pathTemp, hTemp) <- Temp.mkstemps "" ".markdown"
  LB.hPut hTemp input
  hClose hTemp

  editor <- getEnvDefault "EDITOR" "/usr/bin/editor"
  callProcess editor [pathTemp]

  content <- LB.readFile pathTemp
  removeLink pathTemp
  return content

addActivity :: Activity -> Mail -> Mail
addActivity act = addPart (catMaybes [fallbackPart act, Just (activityPart act)])

noteActivity :: Address -> DateTime -> Text -> Activity
noteActivity addr published text =
  makeActivity actor published
    & acVerb    .~ Just ("post" :: Text)
    & acObject  .~ Just note
    & acContent .~ Just text
  where
    actor = toActor addr
    note = emptyObject
      & oAuthor    .~ Just actor
      & oContent   .~ Just text
      & oPublished .~ Just published

likeActivity :: Address -> DateTime -> Activity -> Activity
likeActivity addr published act =
  makeActivity actor published
    & acVerb    .~ Just ("like" :: Text)
    & acObject  .~ Just obj
    & acContent .~ fmap ((<> "\n\n+1") . quote) (fallback act)
  where
    actor = toActor addr
    obj = (asObject act) & oObjectType .~ (Just "activity" :: Maybe Text)

activities :: Mail -> [Activity]
activities m = catMaybes (map parse (filter isActivity (join (mailParts m))))
  where
    isActivity part = partType part == "application/stream+json"
    parse = decode . partContent

toActor :: Address -> Object
toActor addr =
  emptyObject
    & oDisplayName .~ addressName addr
    & oId          .~ Just (Text.pack (show (toUri addr)))

instance Identifiable Address where
  toUri addr = URI
    { uriScheme = "mailto:"
    , uriAuthority = Nothing
    , uriPath = Text.unpack (addressEmail addr)
    , uriQuery = ""
    , uriFragment = ""
    }

instance Identifiable Object where
  toUri o = fromJust $ fmap Text.unpack (o ^. oId) >>= parseURI

instance Identifiable Activity where
  toUri o = fromJust $ fmap Text.unpack (o ^. acId) >>= parseURI

activityPart :: Activity -> Part
activityPart act = Part
  { partType = "application/stream+json"
  , partEncoding = QuotedPrintableText  -- TODO: Do I need to do the encoding?
  , partFilename = Nothing
  , partHeaders = []
  , partContent = encode act
  }

fallbackPart :: Activity -> Maybe Part
fallbackPart act = fmap (\text -> Part
  { partType = "text/plain"
  , partEncoding = QuotedPrintableText  -- TODO: Do I need to do the encoding?
  , partFilename = Nothing
  , partHeaders = []
  , partContent = encodeUtf8 (fromChunks [text])
  }) (fallback act)

fallback :: Activity -> Maybe Text
fallback = (^. acContent)

quote :: Text -> Text
quote msg = "> " <> Text.replace "\n" "\n> " msg
