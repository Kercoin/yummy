module Store
    ( store
    , retrieve
    ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import           Database.Redis
import           System.Environment         (getEnv, lookupEnv)

store :: ByteString -> ByteString -> IO (Either String ())
store key value = do
  conn <- connect =<< getConnectionSettings
  fmap (either replyResult statusResult) <$> runRedis conn $ set key value

retrieve :: ByteString -> IO (Either String ByteString)
retrieve key = do
  conn <- connect =<< getConnectionSettings
  fmap (either replyResult returnResult) <$> runRedis conn $ get key

replyResult :: Reply -> Either String a
replyResult (SingleLine s) = Left (C.unpack s)
replyResult (Error e)      = Left (C.unpack e)
replyResult (Integer i)    = Left (show i)
replyResult (Bulk _)       = Left "Bulk"
replyResult (MultiBulk _)  = Left "MultiBulk..."

returnResult :: Maybe ByteString -> Either String ByteString
returnResult = maybe (Left "no result") Right

statusResult :: Status -> Either String ()
statusResult Ok         = Right ()
statusResult Pong       = Left "pong..."
statusResult (Status s) = Left (C.unpack s)

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
