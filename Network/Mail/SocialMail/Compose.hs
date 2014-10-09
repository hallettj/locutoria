{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Compose where

import Data.Aeson (decode, encode)
import Data.DateTime (DateTime)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, replace, unpack)
import Data.Text.Lazy (fromChunks)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Codec.ActivityStream as AS
import Codec.ActivityStream.Schema (SchemaVerb(..), SchemaObjectType(..))
import Control.Monad (join)
import Network.Mail.Mime (Address(..), Encoding(..), Mail(..), Part(..), addPart)
import Network.URI (URI(..), URIAuth(..))

type Object = AS.Object SchemaObjectType
type Activity = AS.Activity SchemaVerb SchemaObjectType

-- composeNote :: Address -> Text -> Mail

addActivity :: Activity -> Mail -> Mail
addActivity act = addPart (catMaybes [fallbackPart act, Just (activityPart act)])

noteActivity :: Address -> DateTime -> Text -> Activity
noteActivity addr published text = minimal { AS._acVerb = Just Post
                                           , AS._acObject = Just note
                                           , AS._acContent = Just text
                                           }
  where
    actor = toActor addr
    minimal = AS.makeMinimalActivity actor published
    note = AS.emptyObject { AS._oAuthor = Just actor
                          , AS._oContent = Just text
                          , AS._oPublished = Just published
                          }   

likeActivity :: Address -> DateTime -> Activity -> Maybe Activity
likeActivity addr published act = fmap (\o -> minimal
  { AS._acVerb = Just Like
  , AS._acObject = Just o
  , AS._acContent = fmap ((<> "\n\n+1") . quote) (fallback act)
  }) obj
  where
    obj = AS._acObject act
    actor = toActor addr
    minimal = AS.makeMinimalActivity actor published

activities :: Mail -> [Activity]
activities m = catMaybes (map parse (filter isActivity (join (mailParts m))))
  where
    isActivity part = partType part == "application/stream+json"
    parse = decode . partContent

toActor :: Address -> Object
toActor addr = AS.emptyObject { AS._oDisplayName = addressName addr
                              , AS._oURL = Just (toUri (addressEmail addr))
                              }                   

-- Converts an email address to a URI.  This function mirrors the behavior of
-- `parseURI`.
toUri :: Text -> URI
toUri addr = URI
  { uriScheme = "mailto:"
  , uriAuthority = Nothing
  , uriPath = unpack addr
  , uriQuery = ""
  , uriFragment = ""
  }

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
fallback = AS._acContent

quote :: Text -> Text
quote msg = "> " <> replace "\n" "\n> " msg
