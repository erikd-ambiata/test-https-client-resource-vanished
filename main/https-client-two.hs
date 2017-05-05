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

import           System.Environment
import           System.Exit

main :: IO ()
main = do
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
    Right len -> print len

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
  | otherwise                                                            = abortException "someExceptionHandler" se

httpExceptionHandler :: HttpExceptionContent -> IO ()
httpExceptionHandler ex =
  case ex of
    InternalException x -> internalExceptionHandler x
    _ -> pure ()
  where
    internalExceptionHandler :: SomeException -> IO ()
    internalExceptionHandler se =
      abortException "internalExceptionHandler" se

abortException :: String -> SomeException -> IO ()
abortException name se = do
  putStrLn $ name ++ " : " ++ show se
  exitFailure

