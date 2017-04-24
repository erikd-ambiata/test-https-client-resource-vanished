{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HT

httpsPort :: Int
httpsPort = 3000


main :: IO ()
main = do
  let settings = setPort httpsPort $ setHost "*4" defaultSettings
      tlsSettings' = tlsSettings "certificate.pem" "key.pem"
  putStrLn $ "WarpTLS HTTPS server running on port " <> show httpsPort <> "."
  runTLS tlsSettings' settings serverApp

-- Server gives a standard response regardless of Request.
serverApp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serverApp _req respond =
  respond $ responseFile status200 respHeaders "lorem-ipsum.txt" Nothing
  where
    respHeaders =
      [ (HT.hContentType, "text/plain")
      , (CI.mk "X-server-type", "HTTPS")
      ]
