{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.MailingList where

import           Control.Lens (makeLenses)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.List (nubBy, zipWith4)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import           Network.URI (URI, parseURI)
import           System.Directory (doesFileExist)
import           System.IO (Handle, IOMode(..), withFile)
import           System.Process (CreateProcess(..), StdStream(..), createProcess, proc)

data MailingList = MailingList
  { _mlId        :: MailingListId
  , _post        :: [URI]
  , _unsubscribe :: [URI]
  , _archive     :: [URI]
  }
  deriving Show

instance Eq MailingList where
  a == b = _mlId a == _mlId b
instance Ord MailingList where
  compare a b = compare (_mlId a) (_mlId b)

type Header = String
type MailingListId = T.Text

makeLenses ''MailingList

getMailingLists :: [FilePath] -> IO [MailingList]
getMailingLists paths = do
  matches <- grepHeaders "List-Id" paths
  let uniqs        = nubBy (\a b -> snd a == snd b) matches
  let (files, ids) = unzip uniqs
  let sfiles       = map T.unpack files
  posts        <- mapM getPostUris sfiles
  unsubscribes <- mapM getUnsubscribeUris sfiles
  archives     <- mapM getArchiveUris sfiles
  return $ zipWith4 MailingList ids posts unsubscribes archives

getPostUris, getUnsubscribeUris, getArchiveUris :: FilePath -> IO [URI]
getPostUris        = grepHeaderUris "List-Post"
getUnsubscribeUris = grepHeaderUris "List-Unsubscribe"
getArchiveUris     = grepHeaderUris "List-Archive"

parseURIs :: T.Text -> [URI]
parseURIs t = catMaybes (map (parseURI . T.unpack . uri) parts)
  where
    parts = map T.strip $ T.split (== ',') t
    uri p = T.drop 1 (T.takeWhile (/= '>') (T.dropWhile (/= '<') p))

grepHeaders :: Header -> [FilePath] -> IO [(T.Text, T.Text)]
grepHeaders header paths = do
  h <- grepRec header paths >>= insertDelim header delim
  extractMatches delim h
  where
    delim = "@delim@"

grepHeader :: Header -> FilePath -> IO (Maybe T.Text)
grepHeader header path = do
  fileExists <- doesFileExist path
  matches <- if fileExists
    then
      withFile path ReadMode $ \h ->
        combineMultilineHeaders h >>= grep header >>= insertDelim header delim >>= extractMatches delim
    else
      return []
  return $ fmap snd (listToMaybe matches)
  where
    delim = "@delim@"

grepHeaderUris :: Header -> FilePath -> IO [URI]
grepHeaderUris header path = do
  v <- grepHeader header path
  return $ maybe [] parseURIs v

grepRec :: Header -> [FilePath] -> IO Handle
grepRec header paths = do
  (_, Just out, _, _) <-
    createProcess (proc "grep" (["-Ri", header ++ ": "] ++ paths)) { std_out = CreatePipe }
  return out

grep :: Header -> Handle -> IO Handle
grep header h = do
  (_, Just out, _, _) <-
    createProcess (proc "grep" ["-i", header ++ ": "]) { std_in = UseHandle h, std_out = CreatePipe }
  return out

combineMultilineHeaders :: Handle -> IO Handle
combineMultilineHeaders h = do
  (_, Just out, _, _) <-
    createProcess (proc "sed" [":a;/^[^\\t]/{N;s/\\n\\t//;ba}"]) { std_in = UseHandle h, std_out = CreatePipe }
  return out

insertDelim :: Header -> String -> Handle -> IO Handle
insertDelim header delim h = do
  (_, Just out, _, _) <-
    createProcess (proc "sed" [pattern]) { std_in = UseHandle h, std_out = CreatePipe }
  return out
  where
    pattern = "s/:\\?" ++ header ++ ":\\s*/" ++ delim ++ "/i"

extractMatches :: String -> Handle -> IO [(T.Text, T.Text)]
extractMatches delim h = do
  out <- TIO.hGetContents h
  return $ map splitAtDelim (TL.lines out)
  where
    tdelim = T.pack delim
    splitAtDelim l = case T.breakOn tdelim (toStrict l) of
      (file, match) -> (file, T.drop (T.length tdelim) match)
    toStrict t = T.concat (TL.toChunks t)
