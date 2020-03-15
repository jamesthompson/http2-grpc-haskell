{-# LANGUAGE BangPatterns #-}

module Network.GRPC.Server.Helpers where

import qualified Data.ByteString.Char8 as ByteString
import Data.IORef
import Network.GRPC.HTTP2.Types (GRPCStatus (..), HeaderKey, HeaderValue, grpcMessageH, grpcStatusH, trailerForStatusCode)
import Network.Wai (Request)

-- | Helper to set the GRPCStatus on the trailers reply.
modifyGRPCStatus :: IORef [(HeaderKey, HeaderValue)] -> Request -> GRPCStatus -> IO ()
modifyGRPCStatus ref _ (GRPCStatus s msg) =
  writeIORef ref trailers
  where
    !trailers = if ByteString.null msg then [status] else [status, message]
    status = (grpcStatusH, trailerForStatusCode s)
    message = (grpcMessageH, msg)
