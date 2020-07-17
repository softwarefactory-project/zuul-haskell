module Main (main) where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Zuul
import Zuul.Status as Zuul

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/Status.json"
  defaultMain (tests dataFile)

tests :: ByteString -> TestTree
tests dataFile = testGroup "Tests" [encodingTests dataFile]

encodingTests :: ByteString -> TestTree
encodingTests dataFile =
  testGroup
    "FromJSON"
    [ testCase "Test Status.json"
        $ assertBool "ZuulSTatus is decoded"
        $ isStatus (decode dataFile)
    ]
  where
    isStatus :: Maybe Zuul.Status -> Bool
    isStatus (Just _) = True
    isStatus Nothing = False
