{-# LANGUAGE BangPatterns #-}

module Network.GRPC.Server.Helpers where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as ByteString
import Data.IORef
import Network.GRPC.HTTP2.Types (GRPCStatus (..), HeaderKey, HeaderValue, grpcMessageH, grpcStatusH, trailerForStatusCode)
import Network.Wai (Request)

-- | Helper to set the GRPCStatus on the trailers reply.
modifyGRPCStatus :: MonadIO m => IORef [(HeaderKey, HeaderValue)] -> Request -> GRPCStatus -> m ()
modifyGRPCStatus ref _ (GRPCStatus s msg) =
  liftIO $ writeIORef ref trailers
  where
    !trailers = if ByteString.null msg then [status] else [status, message]
    status = (grpcStatusH, trailerForStatusCode s)
    message = (grpcMessageH, msg)
