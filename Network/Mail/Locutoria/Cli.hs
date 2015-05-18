{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mail.Locutoria.Cli where

import           Control.Lens (makeLenses)
import           Graphics.Vty.Widgets.All hiding (Handler)

import           Network.Mail.Locutoria.Notmuch
import           Network.Mail.Locutoria.State

type Channels = List (Maybe Channel) FormattedText
type Threads  = List ThreadId FormattedText

data Cli = Cli
  { _collection     :: Collection
  , _channelsWidget :: Widget Channels
  , _threadsWidget  :: Widget Threads
  }

makeLenses ''Cli
