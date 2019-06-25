{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text     as T
import           Data.Time
import           GHC.Generics

import           Utils

-----------------------------------------------------------------

-- | Wrapper for user token from Developer's Dashboard, https://api.monobank.ua/
type Token = T.Text

-- | Data type that represents currency pair from Monobank
--   at spicific time
data CurrencyPair =
  CurrencyPair
    { cpCurrencyCodeA :: Int     -- ^ Currency code from international identificator
    , cpCurrencyCodeB :: Int     -- ^ Currency code from international identificator
    , cpDate          :: UTCTime -- ^ Timestamp for the currency pair information
    , cpRateSell      :: Float   -- ^ Rate to sell currency
    , cpRateBuy       :: Float   -- ^ Rate to buy currency
    , cpRateCross     :: Float   -- ^
    } deriving (Eq, Show, Generic)

instance FromJSON CurrencyPair where
  parseJSON = genericParseJSON opts
    where
      opts = defaultOptions { fieldLabelModifier = drop 2}
