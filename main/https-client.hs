{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import qualified Data.Conduit.List as DCL
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Conduit
import           Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = do
  request <- parseRequest "https://localhost:3000/"
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- newManager settings
  runTest manager request


runTest :: Manager -> Request -> IO ()
runTest manager request = do
  len <- runResourceT $ do
            resp <- http request manager
            -- Just calculate and return the body length.
            responseBody resp $$+- DCL.fold (\ len bs -> len + BS.length bs) 0
  print len
