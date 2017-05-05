{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import qualified Data.Conduit.List as DCL
import           Network.Connection -- (TLSSettings (..))
import           Network.HTTP.Conduit
import           Network.TLS (TLSError, TLSException)

import           System.Environment
import           System.Exit

main :: IO ()
main = do
  putStrLn "Running https-client-two"
  hostaddr <- head <$> getArgs
  request <- parseRequest $ "https://" ++ hostaddr ++ ":3000/"
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- newManager settings
  forever $ runTest manager request


runTest :: Manager -> Request -> IO ()
runTest manager request = do
  res <- tryIO $ runBodyLength manager request
  case res of
    Left se -> someExceptionHandler se
    Right _ -> pure ()

runBodyLength :: Manager -> Request -> IO Int
runBodyLength manager request =
  runResourceT $ do
    resp <- http request manager
    -- Just calculate and return the body length.
    responseBody resp $$+- DCL.fold (\ len bs -> len + BS.length bs) 0


tryIO :: IO a -> IO (Either SomeException a)
tryIO action = withAsync action waitCatch


someExceptionHandler :: SomeException -> IO ()
someExceptionHandler se
  | Just (HttpExceptionRequest _ e :: HttpException) <- fromException se = httpExceptionHandler e
  | Just (e :: IOException)                          <- fromException se = putStrLn $ "IOException : " ++ show e
  | Just (e :: TLSError)                             <- fromException se = tlsErrorHandler e
  | Just (e :: TLSException)                         <- fromException se = putStrLn $ "TLSException : " ++ show e
  | otherwise                                                            = abortException "someExceptionHandler" se


tlsErrorHandler :: TLSError -> IO ()
tlsErrorHandler e = putStrLn $ "TLSError : " ++ show e


httpExceptionHandler :: HttpExceptionContent -> IO ()
httpExceptionHandler ex =
  case ex of
    InternalException x -> internalExceptionHandler x
    _ -> pure ()
  where
    internalExceptionHandler :: SomeException -> IO ()
    internalExceptionHandler se
      | Just (e :: IOException)  <- fromException se = putStrLn $ "HttpException : " ++ show e
      | Just (e :: TLSError)     <- fromException se = putStrLn $ "HttpException (TLSError) : " ++ show e
      | Just (e :: TLSException) <- fromException se = putStrLn $ "HttpException (TLSException) : " ++ show e
      | otherwise                                    = pure ()


abortException :: String -> SomeException -> IO ()
abortException name se = do
  putStrLn $ name ++ " : " ++ show se
  exitFailure

