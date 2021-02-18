module Main (main) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Zuul

main :: IO ()
main = defaultMain (testGroup "Tests" [encodingTests])

encodingTests :: TestTree
encodingTests =
  testGroup
    "FromJSON"
    [ testCase "Test Status.json" (decodeFp "Status.json" isStatus),
      testCase "Test Tenants.json" (decodeFp "Tenants.json" isTenants),
      testCase "Test Projects.json" (decodeFp "Projects.json" isProjects),
      testCase "Test Jobs.json" (decodeFp "Jobs.json" isJobs),
      testCase "Test Job.json" (decodeFp "Job.json" isJobConfigs)
    ]
  where
    decodeFp fp check = do
      fContent <- BSL.readFile ("./test/data/" <> fp)
      assertBool (fp <> " is decoded") (check (decode fContent))
    isJobConfigs :: Maybe [Zuul.JobConfig] -> Bool
    isJobConfigs = isJust
    isJobs :: Maybe [Zuul.Job] -> Bool
    isJobs = isJust
    isProjects :: Maybe [Zuul.Project] -> Bool
    isProjects = isJust
    isTenants :: Maybe [Zuul.Tenant] -> Bool
    isTenants = isJust
    isStatus :: Maybe Zuul.Status -> Bool
    isStatus = isJust
