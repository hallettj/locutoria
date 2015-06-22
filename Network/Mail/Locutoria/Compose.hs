{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mail.Locutoria.Compose where

import           Control.Error
import           Control.Lens hiding (from, to, without)
import           Control.Monad.Trans (liftIO)
import           Data.Aeson (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.DateTime (DateTime)
import           Data.List (nubBy)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (fromChunks)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Codec.ActivityStream.Dynamic
import           Control.Monad (join)
import           Network.Mail.Mime ( Address(..)
                                   , Alternatives
                                   , Encoding(..)
                                   , Mail(..)
                                   , Part(..)
                                   , addPart, renderMail', sendmailCustom)
import           Network.URI (URI(..), parseURI)
import           System.IO (hClose)
import           System.Posix.Env (getEnvDefault)
import           System.Posix.Files (removeLink)
import qualified System.Posix.Temp as Temp
import           System.Process

import Network.Mail.Locutoria.Conversation
import Network.Mail.Locutoria.Identifiable
import Network.Mail.Locutoria.Message

type Headers = [(B.ByteString, Text)]
type SendCommand = [String]

send :: SendCommand -> Mail -> IO ()
send cmd mail = renderMail' mail >>= (\bs -> sendmailCustom "/usr/bin/env" cmd bs)

composeReply :: Address -> Conversation -> IO (Either String Mail)
composeReply from conv = runEitherT $ do
  content <- liftIO $ inputViaEditor ""
  to      <- hoistEither $ replyRecipients from msg
  let cc = replyCcs from to msg
  return $ Mail
    { mailFrom    = from
    , mailTo      = to
    , mailCc      = cc
    , mailBcc     = []
    , mailHeaders = replyHeaders conv msg
    , mailParts   = replyParts conv content
    }
  where
    msg = last (conv^.convMessages)

-- TODO: Do we want to compute recipents based on the last message in the
-- conversation? Or do we want to scan the whole conversation? This depends on
-- how other might drop people from the recipient list when replying, and what
-- the desired behavior is when that happens.

replyRecipients :: Address -> Message -> Either String [Address]
replyRecipients replier msg = do
  from <- msgFrom msg
  to   <- msgTo msg
  return $ nubByAddr $ without [replier] $ from:to

replyCcs :: Address -> [Address] -> Message -> [Address]
replyCcs replier to msg = nubByAddr $ without (replier:to) $ cc
  where
    cc = nonFatal (msgCc msg)

without :: [Address] -> [Address] -> [Address]
without sub as = filter (\(Address _ addr) -> not (addr `elem` subAddrs)) as
  where
    subAddrs = map (\(Address _ addr) -> addr) sub

nubByAddr :: [Address] -> [Address]
nubByAddr = nubBy $ \(Address _ x) (Address _ y) -> x == y

nonFatal :: Either e [a] -> [a]
nonFatal = either (const []) id

replyHeaders :: Conversation -> Message -> Headers
replyHeaders conv msg =
  [ ("Subject", replySubject msg)
  , ("In-Reply-To", formatIds [msg^.msgId])
  , ("References", formatIds $ map _msgId (conv^.convMessages))
  ]

replySubject :: Message -> Text
replySubject msg = "Re: " <> prev
  where
    prev = fromMaybe "(no subject)" $ msgSubject msg

formatIds :: [MessageId] -> Text
formatIds ids = Text.intercalate " " $ map angle ids
  where
    angle s = "<" <> s <> ">"

-- for reference (most preferred alternative goes last):
-- type Alternatives = [Part]

replyParts :: Conversation -> B.ByteString -> [Alternatives]
replyParts _ content = [[textPart]]
  where
    textPart = Part
      { partType = "text/plain"
      , partEncoding = None
      , partFilename = Nothing
      , partHeaders  = []
      , partContent  = LB.fromChunks [content]
      }

inputViaEditor :: B.ByteString -> IO B.ByteString
inputViaEditor prefilled = do
  (pathTemp, hTemp) <- Temp.mkstemps "" ".markdown"
  B.hPut hTemp prefilled
  hClose hTemp

  editor <- getEnvDefault "EDITOR" "/usr/bin/editor"
  callProcess editor [pathTemp]

  content <- B.readFile pathTemp
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
