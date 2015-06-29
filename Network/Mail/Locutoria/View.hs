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

mapChan :: (Maybe Channel -> Maybe Channel) -> View -> View
mapChan f view = case view of
  Root                      -> showChannel Nothing
  ComposeReply chan _       -> showChannel (Just chan)
  ShowChannel chan _        -> showChannel (Just chan)
  ShowConversation chan _ _ -> showChannel (Just chan)
  ShowQueue                 -> ShowQueue
  Quit                      -> Quit
  where
    showChannel chan = case f chan of
      Just chan' -> ShowChannel chan' Nothing
      Nothing    -> Root

mapConv :: (Maybe Conversation -> Maybe Conversation) -> View -> View
mapConv f view = case view of
  Root                         -> Root
  ComposeReply chan conv       -> go chan conv (ComposeReply chan)
  ShowChannel chan conv        -> ShowChannel chan (f conv)
  ShowConversation chan conv _ -> go chan conv (\c -> ShowConversation chan c Nothing)
  ShowQueue                    -> ShowQueue
  Quit                         -> Quit
  where
    go chan conv v = case f (Just conv) of
      Just c  -> v c
      Nothing -> ShowChannel chan Nothing

mapMsg :: (Maybe Message -> Maybe Message) -> View -> View
mapMsg f view = case view of
  Root                           -> Root
  ComposeReply chan conv         -> ComposeReply chan conv
  ShowChannel chan conv          -> ShowChannel chan conv
  ShowConversation chan conv msg -> ShowConversation chan conv (f msg)
  ShowQueue                      -> ShowQueue
  Quit                           -> Quit
