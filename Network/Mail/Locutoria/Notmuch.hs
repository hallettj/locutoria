{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- This is intended to emulate the api of Foreign.Notmuch; to be used as a shim
-- until I get that library working.
module Network.Mail.Locutoria.Notmuch where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
-- import Codec.MIME.Parse (parseMIMEMessage)
-- import Codec.MIME.Type (MIMEParam(..), MIMEValue(..))
import           Data.Aeson ( FromJSON(..)
                            , ToJSON(..)
                            , Value
                            , fromJSON
                            , object
                            , (.=)
                            , (.:)
                            , (.:?)
                            )
import qualified Data.Aeson as Ae
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import Data.CaseInsensitive (mk)
-- import Data.Char (ord)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector ((!))
import qualified Data.Vector as V
import Network.URI (URI(..))
import System.IO
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, readProcess)

import Network.Mail.Locutoria.Identifiable

data Database = Database { dLoc :: FilePath, dMode :: DatabaseMode  }
  deriving Show

data DatabaseMode =
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
  deriving (Enum, Show)

data Query = Query { qDb :: Database, qText :: String }

type ThreadId = String
data Thread = Thread { tId :: ThreadId }
  deriving Eq

type MessageId = Text

databaseOpen :: FilePath -> DatabaseMode -> IO Database
databaseOpen f m = return (Database f m)

queryCreate :: Database -> String -> IO Query
queryCreate db q = return (Query db q)

queryThreads :: Query -> IO [Thread]
queryThreads q = do
  out <- notmuch ["search", "--output=threads", qText q]
  return $ map Thread (lines out)

threadGetAuthors :: Thread -> IO [Text]
threadGetAuthors = threadGetHeaderValues "From"

threadGetRecipients :: Thread -> IO [Text]
threadGetRecipients = threadGetHeaderValues "To"

threadGetMessages :: Thread -> IO [MsgThread]
threadGetMessages t = do
  out <- notmuchBS ["show", "--format=json", "--body=false", tId t]
  let parsed = Ae.eitherDecode out :: Either String [[MsgThread]]
  case parsed of
    Left err -> putStrLn err >> return []
    Right t ->
      return $ head t

threadGetHeaderValues :: Text -> Thread -> IO [Text]
threadGetHeaderValues h t = do
  ms <- flatThread <$> threadGetMessages t
  return $ catMaybes $ map (lookup h . msgHeaders) ms

-- TODO: use a Foldable instance
flatThread :: [MsgThread] -> [Message]
flatThread = (>>= tMsgs)
  where
    tMsgs t = mtMsg t : flatThread (mtReplies t)

notmuch :: [String] -> IO String
notmuch args = readProcess "/usr/bin/notmuch" opts ""
  where
    opts = ["--config=/home/jesse/.notmuch-galois"] ++ args

notmuchBS :: [String] -> IO BS.ByteString
notmuchBS args = do
  (_, Just hout, _, _) <-
    createProcess (proc "/usr/bin/notmuch" opts) { std_out = CreatePipe }
  BS.hGetContents hout
  where
    opts = ["--config=/home/jesse/.notmuch-galois"] ++ args


data MsgThread = MsgThread
  { mtMsg :: Message
  , mtReplies :: [MsgThread]
  }
  deriving Show

data Message = Message
  { msgId :: MessageId
  , msgHeaders :: [(Text, Text)]
  , msgBody :: [MessagePart]
  }
  deriving Show

data MessagePart = MessagePart
  { mpId :: Int
  , mpContentType :: Text
  , mpSubparts :: [MessagePart]
  }
  deriving (Eq, Show)

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
    where
  parseJSON _ = fail "error parsing Message: expected object"

instance FromJSON MessagePart where
  parseJSON (Ae.Object o) =
    MessagePart <$> o .: "id"
                <*> o .: "content-type"
                <*> subparts
    where
      content = HM.lookup "content" o
      subparts = case content of
        Just ps@(Ae.Array _) -> parseJSON ps
        Nothing -> pure []

  parseJSON _ = fail "error parsing MessagePart: expected object"

-- per RFC 2392
instance Identifiable Message where
  toUri m = URI
    { uriScheme = "mid:"
    , uriAuthority = Nothing
    , uriPath = unpack (msgId m)
    , uriQuery = ""
    , uriFragment = ""
    }

-- per RFC 2392
instance Identifiable (Message, MessagePart) where
  toUri (m, p) = URI
    { uriScheme = "mid:"
    , uriAuthority = Nothing
    , uriPath = unpack (msgId m) ++ "/" ++ show (mpId p)
    , uriQuery = ""
    , uriFragment = ""
    }
