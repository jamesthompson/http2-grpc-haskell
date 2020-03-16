module Network.GRPC.HTTP2.Api where

import Network.GRPC.HTTP2.Types (HeaderValue)

-- |  A class to represent RPC information
class IsRPC t where
  -- | Returns the HTTP2 :path for a given RPC.
  path :: t -> HeaderValue
