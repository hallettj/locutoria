{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.Locutoria.Client where

import           Control.Applicative ((<$>))
import           Control.Event.Handler (AddHandler, Handler)
import           Control.Lens ((^.), (.~), (%~), (&))
import           Data.Default (def)
import           Data.Text (Text)
import           Reactive.Banana (Moment, compile, mapAccum)
import           Reactive.Banana.Frameworks ( Frameworks
                                            , actuate
                                            , changes
                                            , fromAddHandler
                                            , newAddHandler
                                            , pause
                                            , reactimate
                                            , reactimate'
                                            )

import           Network.Mail.Locutoria.Index
import           Network.Mail.Locutoria.Notmuch
import           Network.Mail.Locutoria.State

data Config = Config
  { clDb :: Database
  }

data Ui = Ui { _runUi :: IO (), _updateUi :: State -> IO () }

type Action = Handler Event -> IO ()

data Event = Refresh
           | DoneRefresh
           | ComposeReply ThreadId
           | GenericError Text
           | SetChannel Channel
           | SetThread (Maybe ThreadId)
           | IndexUpdate (Index -> Index)
           | Quit
           | RawMessage Text

locutoria :: Config
          -> (Handler Event -> State -> IO Ui)
          -> IO ()
locutoria config getUi = do
  (addEvent, fireEvent) <- newAddHandler
  (Ui runUi updateUi)   <- getUi fireEvent def
  network <- compile $
    networkDescription config addEvent fireEvent updateUi
  actuate network
  runUi
  pause network

networkDescription :: forall t. Frameworks t
                   => Config
                   -> AddHandler Event
                   -> Handler Event
                   -> (State -> IO ())
                   -> Moment t ()
networkDescription config addEvent fireEvent updateUi = do
  incomingEvents <- fromAddHandler addEvent
  let
    initState        = def
    handler          = handle config <$> incomingEvents
    (actions, state) = mapAccum initState handler
    ui               = updateUi <$> state
  reactimate (fmap ($ fireEvent) actions)
  uiUpdates <- changes ui
  reactimate' uiUpdates

handle :: Config -> Event -> State -> (Action, State)
handle config event s = case event of
  Refresh ->
    if not (s^.refreshing)
    then (\fire -> (do
      cs <- fetchRecentConversations db
      fire $ IndexUpdate cs
      fire $ DoneRefresh
      ), s & refreshing .~ True)
    else
      (noAction, s)

  DoneRefresh -> (noAction, s & refreshing .~ False)

  IndexUpdate f -> (noAction, s & index %~ f)

  SetChannel chan -> (noAction, s { _selectedChannel = chan })
  SetThread threadId -> (noAction, s { _selectedThread = threadId })

  ComposeReply threadId -> (noAction, s { _activity = Compose threadId })

  -- ComposeReply tId -> action s $ \fire -> do
  --   msg <- composeReply (Text.pack tId)
  --   case msg of
  --     Left  err  -> putStrLn (show err)
  --     Right mail -> send mail >> fire Refresh

  Quit -> (noAction, s { _activity = Shutdown })

  where
    db       = clDb config
    noAction = const (return ())


instance Show Event where
  show Refresh                 = "Refresh"
  show DoneRefresh             = "DoneRefresh"
  show (SetChannel chan)       = "SetChannel " ++ show chan
  show (IndexUpdate _)         = "IndexUpdate"
  show (ComposeReply threadId) = "ComposeReply " ++ show threadId
  show Quit                    = "Quit"
  show _                       = "Uknown Event"
