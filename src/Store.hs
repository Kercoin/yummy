module Store
    ( store
    , retrieve
    ) where

import Slack (TeamId, Token(..)) -- move datatypes to a Models module

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Database.Redis
import System.Environment (getEnv, lookupEnv)

store :: TeamId -> Token -> IO (Either String ())
store _       Unknown      = return $ fail "Nope"
store teamId (Token token) = do
  conn <- connect =<< getConnectionSettings
  fmap (either replyResult statusResult) <$> runRedis conn $ set (key teamId) (C.pack token)

replyResult :: Reply -> Either String a
replyResult (SingleLine s) = Left (C.unpack s)
replyResult (Error e)      = Left (C.unpack e)
replyResult (Integer i)    = Left (show i)
replyResult (Bulk _)       = Left "Bulk"
replyResult (MultiBulk _)  = Left "MultiBulk..."

returnResult :: (a -> b) -> Maybe a -> Either String b
returnResult f (Just a) = Right (f a)
returnResult _ Nothing  = Left "no result"

statusResult :: Status -> Either String ()
statusResult Ok         = Right ()
statusResult Pong       = Left "pong..."
statusResult (Status s) = Left (C.unpack s)

retrieve :: TeamId -> IO (Either String Token)
retrieve teamId = do
  conn <- connect =<< getConnectionSettings
  fmap (either replyResult (returnResult (Token . C.unpack))) <$> runRedis conn $ get (key teamId)

key :: TeamId -> ByteString
key teamId = C.pack $ "access-token." ++ teamId

getConnectionSettings :: IO ConnectInfo
getConnectionSettings = connectionSettings <$> getRedisHost
                                           <*> getRedisPort
                                           <*> tryGetRedisPassword

connectionSettings :: HostName -> PortID -> Maybe ByteString -> ConnectInfo
connectionSettings host port pass = defaultConnectInfo { connectHost = host
                                                       , connectPort = port
                                                       , connectAuth = pass
                                                       }

getRedisHost :: IO HostName
getRedisHost = getEnv "YUMMY_REDIS_HOST"

getRedisPort :: IO PortID
getRedisPort = PortNumber . fromInteger . read <$> getEnv "YUMMY_REDIS_PORT"

tryGetRedisPassword :: IO (Maybe ByteString)
tryGetRedisPassword = fmap C.pack <$> lookupEnv "YUMMY_REDIS_AUTH"
