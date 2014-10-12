{-# LANGUAGE OverloadedStrings #-}

-- This is intended to emulate the api of Foreign.Notmuch; to be used as a shim
-- until I get that library working.
module Network.Mail.SocialMail.Notmuch where

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
import qualified Data.HashMap.Strict as HM
import Data.CaseInsensitive (mk)
import Data.Char (ord)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Process (readProcess)

data Database = Database { dLoc :: FilePath, dMode :: DatabaseMode  }
  deriving Show

data DatabaseMode =
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
  deriving (Enum, Show)

data Query = Query { qDb :: Database, qText :: String }

type ThreadId = String
data Thread = Thread { tId :: ThreadId }

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

threadGetHeaderValues :: Text -> Thread -> IO [Text]
threadGetHeaderValues h t = do
  msgs <- fmap lines (notmuch ["search", "--output=files", tId t])
  fmap concat (mapM (parseHeader h) msgs)

notmuch :: [String] -> IO String
notmuch args = readProcess "/usr/bin/notmuch" opts ""
  where
    opts = ["--config=/home/jesse/.notmuch-galois"] ++ args

parseHeader :: Text -> FilePath -> IO [Text]
parseHeader name f = withFile f ReadMode $ \handle -> do
  hSetEncoding handle latin1
  txt <- TIO.hGetContents handle
  -- let txt' = pack (strip (T.unpack txt))
  let val = parseMIMEMessage txt
  let h = filter (\h -> mk (paramName h) == mk name) (mime_val_headers val)
  mapM_ (TIO.putStrLn . paramValue) h
  return $ map paramValue h

-- Strips control codes and extended characters.
strip :: String -> String
strip = filter (\x -> ord x > 32 && ord x < 126 || ord x `elem` [9, 10, 13])



data Message = Message
  { id :: Text
  , headers :: [(Text, Text)]
  , body :: [MessagePart]
  }

data MessagePart = MessagePart
  { id :: Int
  , contentType :: Text
  }

instance FromJSON Message where
  parseJSON (Ae.Object o) =
    Message <$> o .: "id"
            <*> o .: "headers"
            <*> o .: "body"
  parseJSON _ = mzero

instance FromJSON MessagePart where
  parseJSON (Ae.Object o) =
    MessagePart <$> o .: "id"
                <*> o .: "content-type"
  parseJSON _ = mzero
