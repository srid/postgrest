module Config where

import Network.Wai
import Control.Applicative
import Data.Text (strip)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
import Options.Applicative hiding (columns)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))

data AppConfig = AppConfig {
    configDbUri :: String

  , configPort  :: Int
  , configSecure :: Bool
  , configPool :: Int
  , configV1Schema :: String
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db-uri" <> short 'D' <> metavar "URI" <> help "URI of database")

  <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "port number on which to run HTTP server" <> showDefault)
  <*> switch (long "secure" <> short 's' <> help "Redirect all requests to HTTPS")
  <*> option auto (long "db-pool" <> metavar "COUNT" <> value 10 <> help "Max connections in database pool" <> showDefault)
  <*> strOption (long "v1schema" <> metavar "NAME" <> value "1" <> help "Schema to use for nonspecified version (or explicit v1)" <> showDefault)

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    , corsExposedHeaders = Just [
          "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
        , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
        ]
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []
