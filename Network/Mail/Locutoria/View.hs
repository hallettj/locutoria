{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria.View where

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

-- TODO: convert these functions to lenses

viewChannel :: View -> Maybe Channel
viewChannel view = case view of
  Root                      -> Nothing
  ComposeReply chan _       -> Just chan
  ShowChannel chan _        -> Just chan
  ShowConversation chan _ _ -> Just chan
  ShowQueue                 -> Nothing
  Quit                      -> Nothing

viewConversation :: View -> Maybe Conversation
viewConversation view = case view of
  Root                      -> Nothing
  ComposeReply _ conv       -> Just conv
  ShowChannel _ conv        -> conv
  ShowConversation _ conv _ -> Just conv
  ShowQueue                 -> Nothing
  Quit                      -> Nothing

viewMessage :: View -> Maybe Message
viewMessage view = case view of
  Root                     -> Nothing
  ComposeReply _ _         -> Nothing
  ShowChannel _ _          -> Nothing
  ShowConversation _ _ msg -> msg
  ShowQueue                -> Nothing
  Quit                     -> Nothing

mapChan :: (Channel -> Channel) -> View -> View
mapChan f view = case view of
  Root                      -> Root
  ComposeReply chan _       -> ShowChannel (f chan) Nothing
  ShowChannel chan _        -> ShowChannel (f chan) Nothing
  ShowConversation chan _ _ -> ShowChannel (f chan) Nothing
  ShowQueue                 -> ShowQueue
  Quit                      -> Quit

mapConv :: (Conversation -> Conversation) -> View -> View
mapConv f view = case view of
  Root                         -> Root
  ComposeReply chan conv       -> ComposeReply chan (f conv)
  ShowChannel chan conv        -> ShowChannel chan (fmap f conv)
  ShowConversation chan conv _ -> ShowConversation chan (f conv) Nothing
  ShowQueue                    -> ShowQueue
  Quit                         -> Quit

mapMsg :: (Message -> Message) -> View -> View
mapMsg f view = case view of
  Root                           -> Root
  ComposeReply chan conv         -> ComposeReply chan conv
  ShowChannel chan conv          -> ShowChannel chan conv
  ShowConversation chan conv msg -> ShowConversation chan conv (fmap f msg)
  ShowQueue                      -> ShowQueue
  Quit                           -> Quit
