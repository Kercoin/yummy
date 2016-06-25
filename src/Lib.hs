{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Slack
import Store

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8  as C (pack)
import           Data.Text.Lazy         as T (pack)
import           System.Environment          (getEnv)
import           Web.Scotty

startApp :: IO ()
startApp = do
  putStrLn "Starting Server..."
  scotty 8080 routes

routes :: ScottyM ()
routes = do
  get "/" $ html "<body>hello</body>"
  get "/oauth" handleOAuth

handleOAuth :: ActionM ()
handleOAuth = do
  code        <- param "code"
  redirectUri <- liftIO $ baseURL "/oauth"
  OAuthAccessResponse token teamId webhookUrl configurationUrl <- liftIO $ issueSlackToken redirectUri code
  _ <- case token of
         Token t -> liftIO $
              store (C.pack ("access-token." ++ teamId)) (C.pack t)
           >> store (C.pack ("webhook-url."  ++ teamId)) (C.pack webhookUrl)
         Unknown -> fail "No Token"
  redirect $ T.pack configurationUrl

baseURL :: String -> IO String
baseURL path = do
  base <- getEnv "YUMMY_BASE_URL"
  return $ base ++ path
