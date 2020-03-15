module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import GrpcClientExample
import GrpcServerExample

main :: IO ()
main = do
  putStrLn "Running example..."
  server <- async runExampleServer
  threadDelay 2000000
  result <- runExampleClient
  case result of
    Right _ -> do
      putStrLn "Example completed successfully. Shutting down server."
      cancel server
      putStrLn "Exiting."
    Left err -> do
      putStrLn $ "Error encountered: " <> show err
      putStrLn "Shutting down server."
      cancel server
      putStrLn "Exiting."
