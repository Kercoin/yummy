{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Slack

import Control.Monad.IO.Class      (liftIO)
import Data.Text.Lazy         as T (pack)
import System.Environment          (getEnv)
import Web.Scotty

startApp :: IO ()
startApp = do
  putStrLn "Starting Server..."
  scotty 8080 routes

routes :: ScottyM ()
routes = do
  get "/oauth" handleOAuth

handleOAuth :: ActionM ()
handleOAuth = do
  code <- param "code"
  redirectUri <- liftIO $ baseURL "/oauth"
  OAuthAccessResponse _ configurationUrl <- liftIO $ issueSlackToken redirectUri code
  -- TODO: save Token in Redis
  redirect $ T.pack configurationUrl

baseURL :: String -> IO String
baseURL path = do
  base <- getEnv "YUMMY_BASE_URL"
  return $ base ++ path
