{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GrpcClientExample where

import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens.Message (defMessage)
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.ProtoLens (RPC (..))
import Network.HTTP2.Client
import Proto.Examples.Proto.Example

runExampleClient :: IO (Either ClientError ())
runExampleClient = runClientIO $ do
  liftIO $ putStrLn "Attempting to establish connection to server..."
  grpc <- setupGrpcClient (grpcClientConfigSimple "127.0.0.1" 3000 NoTls)
  liftIO $ putStrLn "Sending request to server get endpoint"
  resp <- rawUnary (RPC :: RPC Example "get") grpc defMessage
  liftIO . putStrLn $ "Retrieved this number back from the example server: " <> show resp
