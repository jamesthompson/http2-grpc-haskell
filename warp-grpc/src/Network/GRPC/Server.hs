{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Network.GRPC.Server
  ( runGrpcOnPort,
    runGrpcWithSettings,
    runGrpcWithTLS,
    UnaryHandler,
    ServerStreamHandler,
    ServerStream (..),
    ClientStreamHandler,
    ClientStream (..),
    BiDiStreamHandler,
    BiDiStream (..),
    BiDiStep (..),
    GeneralStreamHandler,
    IncomingStream (..),
    OutgoingStream (..),

    -- * registration
    ServiceHandler,
    unary,
    serverStream,
    clientStream,
    bidiStream,
    generalStream,

    -- * registration
    GRPCStatus (..),
    throwIO,
    GRPCStatusMessage,
    GRPCStatusCode (..),

    -- * to work directly with WAI
    grpcApp,
    grpcService,
  )
where

import Control.Exception (throwIO)
import Network.GRPC.HTTP2.Encoding (Compression)
import Network.GRPC.HTTP2.Types (GRPCStatus (..), GRPCStatusCode (..), GRPCStatusMessage)
import Network.GRPC.Server.Handlers (BiDiStep (..), BiDiStream (..), BiDiStreamHandler, ClientStream (..), ClientStreamHandler, GeneralStreamHandler, IncomingStream (..), OutgoingStream (..), ServerStream (..), ServerStreamHandler, UnaryHandler, bidiStream, clientStream, generalStream, serverStream, unary)
import Network.GRPC.Server.Wai (ServiceHandler (..), grpcApp, grpcService)
import Network.Wai.Handler.Warp (Port, Settings, run, runSettings)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS)

-- * Helpers to constructs and serve a gRPC over HTTP2 application.

runGrpcOnPort ::
  -- | Port
  Port ->
  -- | List of ServiceHandlers. Refer to 'grcpApp'
  [ServiceHandler] ->
  -- | Compression methods used.
  [Compression] ->
  IO ()
runGrpcOnPort port handlers compressions =
  run port (grpcApp compressions handlers)

runGrpcWithSettings ::
  -- | Warp settings.
  Settings ->
  -- | List of ServiceHandler. Refer to 'grcpApp'
  [ServiceHandler] ->
  -- | Compression methods used.
  [Compression] ->
  IO ()
runGrpcWithSettings settings handlers compressions =
  runSettings settings (grpcApp compressions handlers)

runGrpcWithTLS ::
  -- | TLS settings for the HTTP2 server.
  TLSSettings ->
  -- | Warp settings.
  Settings ->
  -- | List of ServiceHandler. Refer to 'grcpApp'
  [ServiceHandler] ->
  -- | Compression methods used.
  [Compression] ->
  IO ()
runGrpcWithTLS tlsSettings settings handlers compressions =
  runTLS tlsSettings settings (grpcApp compressions handlers)
