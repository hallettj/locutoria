{-# LANGUAGE OverloadedStrings #-}

-- This is intended to emulate the api of Foreign.Notmuch; to be used as a shim
-- until I get that library working.
module Network.Mail.Locutoria.Notmuch where

import Control.Applicative ((<$>))
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, readProcess)

import Network.Mail.Locutoria.Message

data Database = Database { dLoc :: FilePath, dMode :: DatabaseMode  }
  deriving Show

data DatabaseMode =
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
  deriving (Enum, Show)

data Query = Query { qDb :: Database, qText :: String }

type ThreadId = String
data Thread = Thread { tId :: ThreadId }
  deriving (Eq, Show)

type MessageId = Text
type SearchTerm = Text

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

threadGetMessages :: Bool -> Thread -> IO [MsgThread]
threadGetMessages includeBody t = do
  -- out <- notmuchBS ["show", "--format=json", "--body=false", tId t]
  out <- notmuchBS ["show", "--format=json", "--body="++ if includeBody then "true" else "false", tId t]
  let parsed = Ae.eitherDecode out :: Either String [[MsgThread]]
  case parsed of
    Left err -> putStrLn err >> return []
    Right ts ->
      return $ head ts

threadGetMessagesFlat :: Bool -> Thread -> IO [Message]
threadGetMessagesFlat includeBody t = flatThread <$> threadGetMessages includeBody t

threadGetHeaderValues :: Text -> Thread -> IO [Text]
threadGetHeaderValues h t = do
  ms <- flatThread <$> threadGetMessages False t
  return $ catMaybes $ map (lookup h . _msgHeaders) ms

notmuch :: [String] -> IO String
notmuch args = readProcess "/usr/bin/env" opts ""
  where
    opts = ["notmuch", "--config=/home/jesse/.notmuch-config"] ++ args

notmuchBS :: [String] -> IO BS.ByteString
notmuchBS args = do
  (_, Just hout, _, _) <-
    createProcess (proc "/usr/bin/env" opts) { std_out = CreatePipe }
  BS.hGetContents hout
  where
    opts = ["notmuch", "--config=/home/jesse/.notmuch-config"] ++ args
