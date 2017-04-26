{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import qualified Data.Conduit.List as DCL
import           Network.Connection -- (TLSSettings (..))
import           Network.HTTP.Conduit

import           System.IO

main :: IO ()
main = do
  request <- parseRequest "https://localhost:3000/"
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- newManager settings
  forever $ runTest manager request


runTest :: Manager -> Request -> IO ()
runTest manager request = do
  res <- try $ runBodyLength manager request
  case res of
    Left (HttpExceptionRequest _ ex) -> httpExceptionHandler ex
    Left (InvalidUrlException _ _) -> error $ show res
    Right len ->
        if len == 10492720
          then do singleChar '.'
          else print len

httpExceptionHandler :: HttpExceptionContent -> IO ()
httpExceptionHandler ex =
  case ex of
    NoResponseDataReceived -> singleChar '1'
    ResponseTimeout -> singleChar '2'
    ConnectionTimeout -> singleChar '3'
    ConnectionFailure _ -> singleChar '4'
    ResponseBodyTooShort _ _ -> singleChar '5'
    InvalidStatusLine _ -> singleChar '6'
    InvalidHeader _ -> singleChar '7'
    ProxyConnectException _ _ _ -> singleChar '8'
    WrongRequestBodyStreamSize _ _ -> singleChar '9'
    InvalidChunkHeaders -> singleChar 'a'
    IncompleteHeaders -> singleChar 'b'
    HttpZlibException _ -> singleChar 'c'
    TooManyRedirects _ -> singleChar 'd'
    OverlongHeaders -> singleChar 'e'
    TlsNotSupported -> singleChar 'f'
    InvalidDestinationHost _ -> singleChar 'g'
    InvalidProxyEnvironmentVariable _ _ -> singleChar 'h'
    StatusCodeException _ _ -> singleChar 'i'
    ConnectionClosed -> singleChar 'j'

    InternalException x -> internalExceptionHandler x


internalExceptionHandler :: SomeException -> IO ()
internalExceptionHandler ex =
  case fromException ex of
    Just (HostCannotConnect _ _) -> singleChar 'H' >> threadDelay (5 * 1000)
    Nothing -> putStrLn $ "\n" ++ show ex


runBodyLength :: Manager -> Request -> IO Int
runBodyLength manager request =
  runResourceT $ do
    resp <- http request manager
    -- Just calculate and return the body length.
    responseBody resp $$+- DCL.fold (\ len bs -> len + BS.length bs) 0

singleChar :: Char -> IO ()
singleChar c = putChar c >> hFlush stdout
