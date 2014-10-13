{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Compose where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (decode, encode)
import Data.DateTime (DateTime)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, append, replace)
import Data.Text.Lazy (fromChunks)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Codec.ActivityStream.Dynamic
import Control.Monad (join)
import Network.Mail.Mime (Address(..), Encoding(..), Mail(..), Part(..), addPart)
import Network.URI (URI(..), URIAuth(..))

-- composeNote :: Address -> Text -> Mail

addActivity :: Activity -> Mail -> Mail
addActivity act = addPart (catMaybes [fallbackPart act, Just (activityPart act)])

noteActivity :: Address -> DateTime -> Text -> Activity
noteActivity addr published text =
  minimal
    & acVerb    .~ Just ("post" :: Text)
    & acObject  .~ Just note
    & acContent .~ Just text
  where
    actor = toActor addr
    minimal = makeActivity actor published
    note = emptyObject
      & oAuthor    .~ Just actor
      & oContent   .~ Just text
      & oPublished .~ Just published

-- likeActivity :: Address -> DateTime -> Activity -> Maybe Activity
-- likeActivity addr published act = fmap (\o -> minimal
--   { AS._acVerb = Just Like
--   , AS._acObject = Just o
--   , AS._acContent = fmap ((<> "\n\n+1") . quote) (fallback act)
--   }) obj
--   where
--     obj = AS._acObject act
--     actor = toActor addr
--     minimal = AS.makeMinimalActivity actor published

activities :: Mail -> [Activity]
activities m = catMaybes (map parse (filter isActivity (join (mailParts m))))
  where
    isActivity part = partType part == "application/stream+json"
    parse = decode . partContent

toActor :: Address -> Object
toActor addr =
  emptyObject
    & oDisplayName .~ addressName addr
    & oURL         .~ Just (toUri (addressEmail addr))

-- Converts email address to a URI
toUri :: Text -> Text
toUri = append "mailto:"

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
