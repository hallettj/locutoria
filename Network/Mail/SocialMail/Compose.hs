{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Compose where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (decode, encode)
import Data.DateTime (DateTime)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, append, pack, replace, unpack)
import Data.Text.Lazy (fromChunks)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Codec.ActivityStream.Dynamic
import Control.Monad (join)
import Network.Mail.Mime (Address(..), Encoding(..), Mail(..), Part(..), addPart)
import Network.URI (URI(..), parseURI)

import Network.Mail.SocialMail.Identifiable

-- composeNote :: Address -> Text -> Mail

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
    & oId          .~ Just (pack (show (toUri addr)))

instance Identifiable Address where
  toUri addr = URI
    { uriScheme = "mailto:"
    , uriAuthority = Nothing
    , uriPath = unpack (addressEmail addr)
    , uriQuery = ""
    , uriFragment = ""
    }

instance Identifiable Object where
  toUri o = fromJust $ fmap unpack (o ^. oId) >>= parseURI

instance Identifiable Activity where
  toUri o = fromJust $ fmap unpack (o ^. acId) >>= parseURI

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
quote msg = "> " <> replace "\n" "\n> " msg
