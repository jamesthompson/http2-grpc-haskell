{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- * Module for GRPC <> HTTP2 type mappings
module Network.GRPC.HTTP2.Types where

import Control.Exception (Exception)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.CaseInsensitive (CI)
import Data.Maybe (fromMaybe)

-- * Canonical gRPC Header Keys -------------------------------------------------

-- | HTTP2 header key type
type HeaderKey = CI ByteString

-- | grpc-timeout header key
grpcTimeoutH :: HeaderKey
grpcTimeoutH = "grpc-timeout"

-- | grpc-encoding header key
grpcEncodingH :: HeaderKey
grpcEncodingH = "grpc-encoding"

-- | grpc-accept-encoding header key
grpcAcceptEncodingH :: HeaderKey
grpcAcceptEncodingH = "grpc-accept-encoding"

-- | grpc-status header key
grpcStatusH :: HeaderKey
grpcStatusH = "grpc-status"

-- | grpc-message header key
grpcMessageH :: HeaderKey
grpcMessageH = "grpc-message"

-- | Helper for common authorization header
authorizationH :: HeaderKey
authorizationH = "authorization"

-- * Canonical gRPC Header Values -----------------------------------------------

-- | HTTP2 header value type
type HeaderValue = ByteString

-- | grpc-accept-encoding default header value
grpcAcceptEncodingHVdefault :: HeaderValue
grpcAcceptEncodingHVdefault = "identity"

-- | Established gRPC content-type header value
grpcContentTypeHV :: HeaderValue
grpcContentTypeHV = "application/grpc"

-- | Helper for common Bearer style header token values
makeBearerHV :: ByteString -> HeaderValue
makeBearerHV = (<>) "Bearer "

-- * Canonical gRPC Header Values -----------------------------------------------

-- | Possible gRPC response status codes (mapped from inbound integer values as a header key)
--   https://grpc.io/grpc/core/impl_2codegen_2status_8h.html#a35ab2a68917eb836de84cb23253108eb
data GRPCStatusCode
  = OK
  | CANCELLED
  | UNKNOWN
  | INVALID_ARGUMENT
  | DEADLINE_EXCEEDED
  | NOT_FOUND
  | ALREADY_EXISTS
  | PERMISSION_DENIED
  | UNAUTHENTICATED
  | RESOURCE_EXHAUSTED
  | FAILED_PRECONDITION
  | ABORTED
  | OUT_OF_RANGE
  | UNIMPLEMENTED
  | INTERNAL
  | UNAVAILABLE
  | DATA_LOSS
  deriving (Bounded, Eq, Ord, Show)

-- | A gRPC status message represented by a header value
type GRPCStatusMessage = HeaderValue

-- | A gRPC status header mapping a code to a message
data GRPCStatus = GRPCStatus !GRPCStatusCode !GRPCStatusMessage
  deriving (Eq, Ord, Show)

instance Exception GRPCStatus

-- | Datatype representing an invalid gRPC status response code and value
--   (really one not-understood by this library, could be an addition to the spec!)
newtype InvalidGRPCStatus
  = InvalidGRPCStatus [(HeaderKey, HeaderValue)]
  deriving (Eq, Ord, Show)

instance Exception InvalidGRPCStatus

-- | The HTTP2-Authority portion of an URL (e.g., "dicioccio.fr:7777").
type Authority = HeaderValue

-- | Map an enumerated gRPC status code to its integer wire header representation
trailerForStatusCode ::
  GRPCStatusCode ->
  HeaderValue
trailerForStatusCode = \case
  OK ->
    "0"
  CANCELLED ->
    "1"
  UNKNOWN ->
    "2"
  INVALID_ARGUMENT ->
    "3"
  DEADLINE_EXCEEDED ->
    "4"
  NOT_FOUND ->
    "5"
  ALREADY_EXISTS ->
    "6"
  PERMISSION_DENIED ->
    "7"
  UNAUTHENTICATED ->
    "16"
  RESOURCE_EXHAUSTED ->
    "8"
  FAILED_PRECONDITION ->
    "9"
  ABORTED ->
    "10"
  OUT_OF_RANGE ->
    "11"
  UNIMPLEMENTED ->
    "12"
  INTERNAL ->
    "13"
  UNAVAILABLE ->
    "14"
  DATA_LOSS ->
    "15"

-- | Map a gRPC header value integer code to an enumerated GRPCStatusCode value
statusCodeForTrailer ::
  HeaderValue ->
  Maybe GRPCStatusCode
statusCodeForTrailer = \case
  "0" ->
    Just OK
  "1" ->
    Just CANCELLED
  "2" ->
    Just UNKNOWN
  "3" ->
    Just INVALID_ARGUMENT
  "4" ->
    Just DEADLINE_EXCEEDED
  "5" ->
    Just NOT_FOUND
  "6" ->
    Just ALREADY_EXISTS
  "7" ->
    Just PERMISSION_DENIED
  "16" ->
    Just UNAUTHENTICATED
  "8" ->
    Just RESOURCE_EXHAUSTED
  "9" ->
    Just FAILED_PRECONDITION
  "10" ->
    Just ABORTED
  "11" ->
    Just OUT_OF_RANGE
  "12" ->
    Just UNIMPLEMENTED
  "13" ->
    Just INTERNAL
  "14" ->
    Just UNAVAILABLE
  "15" ->
    Just DATA_LOSS
  _ ->
    Nothing

-- | Map a 'GRPCStatus' to wire trailers
trailers ::
  GRPCStatus ->
  [(HeaderKey, HeaderValue)]
trailers (GRPCStatus s msg) =
  if BC8.null msg
    then [status]
    else
      [ status,
        message
      ]
  where
    status = (grpcStatusH, trailerForStatusCode s)
    message = (grpcMessageH, msg)

-- | Read a 'GRPCStatus' from HTTP2 trailers
readTrailers ::
  [(HeaderKey, HeaderValue)] ->
  Either InvalidGRPCStatus GRPCStatus
readTrailers pairs = maybe (Left $ InvalidGRPCStatus pairs) Right $ do
  status <- statusCodeForTrailer =<< lookup grpcStatusH pairs
  return $ GRPCStatus status message
  where
    message = fromMaybe "" (lookup grpcMessageH pairs)

-- * Sundry types ---------------------------------------------------------------

-- | Timeout in seconds
newtype Timeout
  = Timeout Int

showTimeout ::
  Timeout ->
  HeaderValue
showTimeout (Timeout n) =
  BC8.pack $ show (n * 1000000) ++ "u"
