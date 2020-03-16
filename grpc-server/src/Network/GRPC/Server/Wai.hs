{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.Server.Wai where

import Control.Exception (Handler (..), SomeException, catches, throwIO)
import Data.Binary.Builder (Builder)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Lazy (fromStrict)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.GRPC.HTTP2.Encoding (Compression, Decoding (..), Encoding (..), grpcCompressionHV, uncompressed)
import Network.GRPC.HTTP2.Types (GRPCStatus (..), GRPCStatusCode (..), grpcAcceptEncodingH, grpcContentTypeHV, grpcEncodingH, grpcMessageH, grpcStatusH)
import Network.GRPC.Server.Helpers (modifyGRPCStatus)
import Network.HTTP.Types (status200, status404)
import Network.HTTP2.Server (NextTrailersMaker (..))
import Network.Wai (Application, Request (..), rawPathInfo, requestHeaders, responseLBS, responseStream)
import Network.Wai.Handler.Warp (defaultHTTP2Data, http2dataTrailers, modifyHTTP2Data)

type ServiceHandler =
  HM.HashMap ByteString WaiHandler

-- | A Wai Handler for a request.
type WaiHandler =
  -- | Compression for the request input.
  Decoding ->
  -- | Compression for the request output.
  Encoding ->
  -- | Request object.
  Request ->
  -- | Write a data chunk in the reply.
  (Builder -> IO ()) ->
  -- | Flush the output.
  IO () ->
  IO ()

-- | Build a WAI 'Application' from a HashMap of ServiceHandlers.
--
-- Currently, gRPC calls are lookuped up by traversing the list of ServiceHandler.
-- This lookup may be inefficient for large amount of services.
grpcApp ::
  [Compression] ->
  ServiceHandler ->
  Application
grpcApp compressions services =
  grpcService compressions services err404app
  where
    err404app :: Application
    err404app req rep =
      rep
        $ responseLBS status404 []
        $ fromStrict ("not found: " <> rawPathInfo req)

-- | Aborts a GRPC handler with a given GRPCStatus.
closeEarly :: GRPCStatus -> IO a
closeEarly = throwIO

-- | Build a WAI 'Middleware' from a ServiceHandler mapping.
grpcService ::
  [Compression] ->
  ServiceHandler ->
  (Application -> Application)
grpcService compressions services app = \req rep -> do
  r <- newIORef []
  modifyHTTP2Data req $ \h2data ->
    Just $! (fromMaybe defaultHTTP2Data h2data) {http2dataTrailers = trailersMaker r}
  case HM.lookup (rawPathInfo req) services of
    Just handler ->
      -- Handler that catches early GRPC termination and other exceptions.
      --
      -- Other exceptions are turned into GRPC status INTERNAL (rather
      -- than returning a 500).
      --
      -- These exceptions are swallowed from the WAI "onException"
      -- handler, so we'll need a better way to handle this case.
      let grpcHandler write flush =
            doHandle r handler req write flush
              `catches` [ Handler $ \(e :: GRPCStatus) ->
                            modifyGRPCStatus r req e,
                          Handler $ \(e :: SomeException) ->
                            modifyGRPCStatus r req (GRPCStatus INTERNAL $ ByteString.pack $ show e)
                        ]
       in (rep $ responseStream status200 hdrs200 grpcHandler)
    Nothing ->
      app req rep
  where
    hdrs200 =
      [ ("content-type", grpcContentTypeHV),
        ("trailer", CI.original grpcStatusH),
        ("trailer", CI.original grpcMessageH)
      ]
    doHandle r handler req write flush = do
      let bestCompression = lookupEncoding req compressions
      let pickedCompression = fromMaybe (Encoding uncompressed) bestCompression
      let hopefulDecompression = lookupDecoding req compressions
      let pickedDecompression = fromMaybe (Decoding uncompressed) hopefulDecompression
      -- putStrLn "running handler"
      _ <- handler pickedDecompression pickedCompression req write flush
      -- putStrLn "setting GRPC status"
      modifyGRPCStatus r req (GRPCStatus OK "WAI handler ended.")
    trailersMaker r Nothing = Trailers <$> readIORef r
    trailersMaker r _ = return $ NextTrailersMaker (trailersMaker r)

-- | Looks-up header for encoding outgoing messages.
requestAcceptEncodingNames :: Request -> [ByteString]
requestAcceptEncodingNames req =
  maybe [] (ByteString.split ',') $
    lookup grpcAcceptEncodingH (requestHeaders req)

-- | Looks-up the compression to use from a set of known algorithms.
lookupEncoding :: Request -> [Compression] -> Maybe Encoding
lookupEncoding req compressions =
  Encoding
    <$> safeHead
      [ c | c <- compressions, n <- requestAcceptEncodingNames req, n == grpcCompressionHV c
      ]
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Looks-up header for decoding incoming messages.
requestDecodingName :: Request -> Maybe ByteString
requestDecodingName req =
  lookup grpcEncodingH (requestHeaders req)

-- | Looks-up the compression to use for decoding messages.
lookupDecoding :: Request -> [Compression] -> Maybe Decoding
lookupDecoding req compressions = fmap Decoding $ do
  d <- requestDecodingName req
  lookup d [(grpcCompressionHV c, c) | c <- compressions]
