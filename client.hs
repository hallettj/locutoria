{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Mail.SocialMail.Cli

import Network.Mail.SocialMail.Internal

main :: IO ()
main = getListAddrs >>= \cs -> ui ("all" : "" : cs)
