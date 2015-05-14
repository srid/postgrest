module Main where

import Paths_postgrest (version)

import App
import Middleware
import Error(errResponse)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Network.Wai (strictRequestBody)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.URI as URI
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Version (versionBranch)
import qualified Hasql as H
import qualified Hasql.Postgres as P
import Options.Applicative hiding (columns)

import Config (AppConfig(..), argParser, corsPolicy)

extractUserPassFromUriAuth :: URI.URIAuth -> Maybe (String, String)
extractUserPassFromUriAuth auth =
  case splitOn "@" (URI.uriUserInfo auth) of
   (x:_) -> case splitOn ":" x of
             (u:p:_) -> Just (u, p)
             _ -> Nothing
   _ -> Nothing

checkCreds :: URI.URI -> ByteString -> ByteString -> Bool
checkCreds uri user pass =
  case URI.uriAuthority uri of
   Nothing -> True
   Just auth -> case extractUserPassFromUriAuth auth of
     Nothing -> False
     Just (u, p) -> u == (cs user) && p == (cs pass)

main :: IO ()
main = do
  let opts = info (helper <*> argParser) $
                fullDesc
                <> progDesc (
                    "PostgREST "
                    <> prettyVersion
                    <> " / create a REST API to an existing Postgres database"
                )
      parserPrefs = prefs showHelpOnError
  conf <- customExecParser parserPrefs opts
  let port = configPort conf

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgUriString = configDbUri conf
      pgUri = fromJust $ URI.parseURI pgUriString
      pgSettings = P.StringSettings (cs $ pgUriString)
      checkCredsIO = (\u p -> return $ (checkCreds pgUri) u p)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout
        . (if configSecure conf then redirectInsecure else id)
        . basicAuth checkCredsIO "Postgrest realm"
        . gzip def . cors corsPolicy
        . static

  poolSettings <- maybe (fail "Improper session settings") return $
                H.poolSettings (fromIntegral $ configPool conf) 30
  pool :: H.Pool P.Postgres
          <- H.acquirePool pgSettings poolSettings

  runSettings appSettings $ middle $ \req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx Nothing $
      unauthenticated (app (cs $ configV1Schema conf) body) req
    either (respond . errResponse) respond resOrError

  where
    prettyVersion = intercalate "." $ map show $ versionBranch version
