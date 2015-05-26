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
import           System.Exit (exitSuccess)

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
           | GenericError Text
           | SetRoute Route
           | IndexUpdate (Index -> Index)
           | Quit
           | RawMessage Text

locutoria :: Config
          -> (Handler Event -> State -> IO Ui)
          -> IO ()
locutoria config getUi = do
  let initState = def
  (addEvent, fireEvent) <- newAddHandler
  (Ui runUi updateUi)   <- getUi fireEvent initState
  network <- compile $
    networkDescription config addEvent fireEvent updateUi initState
  actuate network
  runUi
  pause network

networkDescription :: forall t. Frameworks t
                   => Config
                   -> AddHandler Event
                   -> Handler Event
                   -> (State -> IO ())
                   -> State
                   -> Moment t ()
networkDescription config addEvent fireEvent updateUi initState = do
  incomingEvents <- fromAddHandler addEvent
  let
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
  SetRoute r -> (noAction, s { _route = r })

  -- ComposeReply tId -> action s $ \fire -> do
  --   msg <- composeReply (Text.pack tId)
  --   case msg of
  --     Left  err  -> putStrLn (show err)
  --     Right mail -> send mail >> fire Refresh

  Quit -> (const exitSuccess, s)

  where
    db       = clDb config
    noAction = const (return ())


instance Show Event where
  show Refresh                 = "Refresh"
  show DoneRefresh             = "DoneRefresh"
  show (SetRoute r)            = "SetRoute " ++ show r
  show (IndexUpdate _)         = "IndexUpdate"
  show Quit                    = "Quit"
  show _                       = "Uknown Event"
