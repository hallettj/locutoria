{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.View where

import           Control.Applicative
import           Control.Lens hiding (view)

import           Network.Mail.Locutoria.Channel
import           Network.Mail.Locutoria.Conversation
import           Network.Mail.Locutoria.Message

data View = Root
          | ComposeReply Channel Conversation
          | ShowChannel Channel (Maybe Conversation)
          | ShowConversation Channel Conversation (Maybe Message)
          | ShowQueue
          | Quit
  deriving (Eq, Ord, Show)

viewChannel :: Traversal' View Channel
viewChannel f view = case view of
  Root                      -> pure view
  ComposeReply chan _       -> switchChannel chan
  ShowChannel chan _        -> switchChannel chan
  ShowConversation chan _ _ -> switchChannel chan
  ShowQueue                 -> pure view
  Quit                      -> pure view
  where
    switchChannel chan = showIfChanged chan <$> f chan
    showIfChanged chan chan' = if chan' == chan
                               then view
                               else ShowChannel chan' Nothing

viewConversation :: Traversal' View Conversation
viewConversation f view = case view of
  Root                         -> pure view
  ComposeReply chan conv       -> ComposeReply chan <$> f conv
  ShowChannel _    Nothing     -> pure view
  ShowChannel chan (Just conv) -> ShowChannel chan . Just <$> f conv
  ShowConversation chan conv _ -> switchIfChanged chan conv <$> f conv
  ShowQueue                    -> pure view
  Quit                         -> pure view
  where
    switchIfChanged chan conv conv' = if conv == conv'
                                      then view
                                      else ShowConversation chan conv' Nothing

viewMessage :: Traversal' View Message
viewMessage f view = case view of
  Root                                  -> pure view
  ComposeReply _ _                      -> pure view
  ShowChannel _ _                       -> pure view
  ShowConversation _    _    Nothing    -> pure view
  ShowConversation chan conv (Just msg) -> ShowConversation chan conv . Just <$> f msg
  ShowQueue                             -> pure view
  Quit                                  -> pure view
