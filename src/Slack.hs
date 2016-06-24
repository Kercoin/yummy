{-# LANGUAGE OverloadedStrings #-}

module Slack
    ( OAuthAccessResponse(..)
    , Token(..)
    , TeamId
    , issueSlackToken
    ) where

import Data.Aeson
import Data.Aeson.Types    (typeMismatch)
import Data.List           (intercalate)
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest)
import System.Environment  (getEnv)

issueSlackToken :: String -> String -> IO OAuthAccessResponse
issueSlackToken redirectUri code = do
  clientId     <- getEnv "YUMMY_CLIENT_ID"
  clientSecret <- getEnv "YUMMY_CLIENT_SECRET"
  let url = mkURL "GET" "https://slack.com/api/oauth.access"
                  [ ( "client_id"    , clientId     )
                  , ( "client_secret", clientSecret )
                  , ( "code"         , code         )
                  , ( "redirect_uri" , redirectUri  )
                  ]
  r <- parseRequest url
  getResponseBody <$> httpJSON r

type URL = String
type Parameters = [Parameter]
type Parameter = (String, String)

mkURL :: String -> URL -> Parameters -> URL
mkURL verb u = mkUrl (verb ++ " " ++ u)

mkUrl :: URL -> Parameters -> URL
mkUrl url []         = url
mkUrl url parameters = url ++ "?" ++ toParameters parameters

toParameters :: Parameters -> String
toParameters = intercalate "&" . map toParameter

toParameter :: Parameter -> String
toParameter (a, b) = a ++ "=" ++ b

type TeamId = String
data Token = Unknown | Token String deriving (Show)

data OAuthAccessResponse = OAuthAccessResponse Token TeamId URL URL deriving Show

instance FromJSON OAuthAccessResponse where
  parseJSON (Object o) = do
    incomingWebhook <- o .: "incoming_webhook"
    OAuthAccessResponse <$> parseJSON (Object o)
                        <*> o .: "team_id"
                        <*> incomingWebhook .: "url"
                        <*> incomingWebhook .: "configuration_url"
  parseJSON invalid = typeMismatch "OAuthAccessResponse" invalid

instance FromJSON Token where
  parseJSON (Object o) = maybe Unknown Token <$> o .:? "access_token"
  parseJSON invalid = typeMismatch "Token" invalid
