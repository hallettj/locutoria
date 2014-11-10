{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.Identifiable where

import Network.URI (URI)

class Identifiable a where
  toUri :: a -> URI

instance Identifiable URI where
  toUri = id
