{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GrpcServerExample where

import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens.Message (defMessage)
import Lens.Family2
import Network.GRPC.HTTP2.Encoding (gzip)
import Network.GRPC.HTTP2.ProtoLens (RPC (..))
import Network.GRPC.Server
import Proto.Examples.Proto.Example
import Proto.Examples.Proto.Example_Fields

runExampleServer :: IO ()
runExampleServer =
  runGrpcOnPort 3000 handlers [gzip]

handlers :: [ServiceHandler]
handlers =
  [unary (RPC :: RPC Example "get") handleGet]

handleGet :: UnaryHandler EmptyMessage IntegerResponse
handleGet _ _ = do
  liftIO $ putStrLn "Handling inbound get request, sending back response"
  pure $ defMessage & myInteger .~ 3142
