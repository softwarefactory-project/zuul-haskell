{-# LANGUAGE FlexibleContexts #-}

-- | Helper functions to derive JSON encoder/decoder
module Zuul.Aeson (zuulParseJSON, zuulToJSON) where

import Data.Aeson (GFromJSON, GToJSON', Options (fieldLabelModifier), Value, Zero, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.Char (isUpper, toLower)
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)

zuulOptions :: T.Text -> Options
zuulOptions prefix = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (T.length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : updateCase' xs
    updateCase' [] = []
    updateCase' (x : xs)
      | isUpper x = '_' : toLower x : updateCase' xs
      | otherwise = x : updateCase' xs

-- | An helper function to implement the parseJSON class
zuulParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => T.Text -> Value -> Parser a
zuulParseJSON = genericParseJSON . zuulOptions

-- | An helper function to implement the toJSON class
zuulToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => T.Text -> a -> Value
zuulToJSON = genericToJSON . zuulOptions
