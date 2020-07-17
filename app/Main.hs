module Main (main) where

import qualified Data.Text as T
import System.Environment (getArgs)
import Zuul (getStatus, withClient)
import qualified Zuul.Status

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url, pipeline, queue] ->
      withClient url $ \client -> do
        status <- getStatus client
        case Zuul.Status.pipelineChanges pipeline (Just queue) status of
          Just changes -> print (map Zuul.Status.changeProject (Zuul.Status.liveChanges changes))
          Nothing -> putStrLn "No pipeline found :("
    _ -> putStrLn "usage: zuul-cli url pipeline queue"
