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
    [ decodeFp "Status.json" isStatus,
      decodeFp "Tenants.json" isTenants,
      decodeFp "Projects.json" isProjects,
      decodeFp "Project.json" isProjectConfig,
      decodeFp "Jobs.json" isJobs,
      decodeFp "Job.json" isJobConfigs
    ]
  where
    decodeFp fp check = testCase ("Test " <> fp) $ do
      fContent <- BSL.readFile ("./test/data/" <> fp)
      assertBool (fp <> " is decoded") (check (decode fContent))
    isProjectConfig :: Maybe Zuul.ProjectConfig -> Bool
    isProjectConfig = isJust
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
