{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Locutoria where

import           Control.Lens

import           Network.Mail.Locutoria.State
import           Network.Mail.Locutoria.Cli.Ui (ui)
import           Network.Mail.Locutoria.Config

locutoria :: Config -> IO ()
locutoria cfg = ui cfg (mkState (cfg^.cfgDb)) >> return ()
