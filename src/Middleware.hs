{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Middleware where

import Data.Monoid (mconcat)
-- import Data.Pool(withResource, Pool)

import qualified Hasql as H
import qualified Hasql.Postgres as P
import Data.String.Conversions(cs)

import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (status400, status301)
import Network.Wai (Application, requestHeaders, responseLBS, rawPathInfo,
                   rawQueryString, isSecure, Request(..), Response)
import Network.URI (URI(..), parseURI)


unauthenticated :: forall s.
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
unauthenticated app req = do
    app req

redirectInsecure :: Application -> Application
redirectInsecure app req respond = do
  let hdrs = requestHeaders req
      host = lookup "host" hdrs
      uriM = parseURI . cs =<< mconcat [
        Just "https://",
        host,
        Just $ rawPathInfo req,
        Just $ rawQueryString req]
      isHerokuSecure = lookup "x-forwarded-proto" hdrs == Just "https"

  if not (isSecure req || isHerokuSecure)
    then case uriM of
              Just uri ->
                respond $ responseLBS status301 [
                    (hLocation, cs . show $ uri { uriScheme = "https:" })
                  ] ""
              Nothing ->
                respond $ responseLBS status400 [] "SSL is required"
    else app req respond
