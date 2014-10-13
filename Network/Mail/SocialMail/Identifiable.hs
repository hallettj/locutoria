{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SocialMail.Identifiable where

import Network.URI (URI)

class Identifiable a where
  toUri :: a -> URI

instance Identifiable URI where
  toUri = id
