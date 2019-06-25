{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Monobank.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text      as T
import           Data.Time
import           GHC.Generics

import           Monobank.Utils

--------------------------------------------------------------------------------

-- | Wrapper for user token from Developer's Dashboard, https://api.monobank.ua/
type Token = T.Text

data Account =
  Account
    { acId           :: T.Text    -- ^
    , acBalance      :: Int       -- ^ Current balance
    , acCreditLimit  :: Int       -- ^ Current credit limit available to user
    , acCurrencyCode :: Int       -- ^ Currency of the account
    , acCashbackType :: T.Text    -- ^
    } deriving (Eq, Show, Generic)

instance FromJSON Account where
   parseJSON = genericParseJSON opts
     where
       opts = defaultOptions { fieldLabelModifier = uncapFst . drop 2 }

data User =
  User
    { uName     :: T.Text    -- ^ User's full name
    , uAccounts :: [Account] -- ^ List of user accounts, by currency type
    } deriving (Eq, Show)

instance FromJSON User where
  parseJSON =
    withObject "object" $ \o -> do
      nm     <- o .:  "name"
      ac     <- o .:  "accounts"

      return $ User nm ac

data Statement =
  Statement
    { stId              :: String   -- ^
    , stTime            :: String   -- ^
    , stDescription     :: String   -- ^
    , stMCC             :: String   -- ^
    , stHold            :: Bool     -- ^
    , stAmount          :: Int      -- ^
    , stOperationAmount :: Int      -- ^
    , stCurrency        :: Int      -- ^
    , stComissionRate   :: Int      -- ^
    , stCashbackAmount  :: Int      -- ^
    , balance           :: Int      -- ^
    } deriving (Eq, Show, Generic, FromJSON)

data Currency =
  Currency
    { isoCode        :: String
    , isoNumericCode :: Int
    , decimalDigits  :: Int
    , symbol         :: String
    , name           :: String
    } deriving (Eq, Show)

-- | Data type that represents currency pair from Monobank
--   at spicific time
data CurrencyPair =
  CurrencyPair
    { cpCurrencyCodeA :: Currency      -- ^ Currency derived from international identificator
    , cpCurrencyCodeB :: Currency      -- ^ Currency derived from international identificator
    , cpDate          :: Int           -- ^ Timestamp for the currency pair information
    , cpRateSell      :: Maybe Float   -- ^ Rate to sell currency
    , cpRateBuy       :: Maybe Float   -- ^ Rate to buy currency
    , cpRateCross     :: Maybe Float   -- ^ Rate
    } deriving (Show)

instance FromJSON CurrencyPair where
  parseJSON =
    withObject "object" $ \o -> do
      currA'     <- o .:  "currencyCodeA"
      currB'     <- o .:  "currencyCodeB"
      date'      <- o .:  "date"         -- TODO: convert to UTCTime
      rateSell'  <- o .:? "rateSell"
      rateBuy'   <- o .:? "rateBuy"
      rateCross' <- o .:? "rateCross"

      let currA = convertCurrencyFromCode currA'
          currB = convertCurrencyFromCode currB'
          --date  =

      return $
        CurrencyPair currA currB date' rateSell' rateBuy' rateCross'

showCurrencyPair :: CurrencyPair -> IO ()
showCurrencyPair cp = do
  putStrLn $ (isoCode $ cpCurrencyCodeA cp) ++ "/" ++ (isoCode $ cpCurrencyCodeB cp)
  case (cpRateBuy cp) of
    Nothing  -> do
      putStrLn $ " - Cross: " ++ (show $ fromMaybe 0 $ cpRateCross cp) ++ (symbol $ cpCurrencyCodeB cp)
      putStrLn $ ""
    Just buy -> do
      putStrLn $ " - Buy:  " ++ (show $ buy) ++ " " ++ (symbol $ cpCurrencyCodeB cp)
      putStrLn $ " - Sell: " ++ (show $ fromMaybe 0 $ cpRateSell cp) ++ (symbol $ cpCurrencyCodeB cp)
      putStrLn $ ""

--------------------------------------------------------------------------------

convertCurrencyFromCode :: Int -> Currency
convertCurrencyFromCode val =
  case val of
    840       ->  Currency { isoCode = "USD", isoNumericCode=  840, decimalDigits= 2, symbol= "$", name="Dollar"}
    978       ->  Currency { isoCode = "EUR", isoNumericCode=  978, decimalDigits= 2, symbol= "€", name="Euro"}
    980       ->  Currency { isoCode = "UAH", isoNumericCode=  980, decimalDigits= 2, symbol= "₴", name="Hrivnya"}
    643       ->  Currency { isoCode = "RUB", isoNumericCode=  643, decimalDigits= 2, symbol= "", name="Ruble"}
    826       ->  Currency { isoCode = "GBP", isoNumericCode=  826, decimalDigits= 2, symbol= "", name="Pound Sterling"}
    949       ->  Currency { isoCode = "TRY", isoNumericCode=  949, decimalDigits= 2, symbol= "", name="Lira"}
    985       ->  Currency { isoCode = "PLN", isoNumericCode=  985, decimalDigits= 2, symbol= "", name="Zloty"}
    348       ->  Currency { isoCode = "HUF", isoNumericCode=  348, decimalDigits= 2, symbol= "", name="Forint"}
    208       ->  Currency { isoCode = "DKK", isoNumericCode=  208, decimalDigits= 2, symbol= "", name="Danish Krone"}
    203       ->  Currency { isoCode = "CZK", isoNumericCode=  203, decimalDigits= 2, symbol= "", name="Czech Koruna"}
    124       ->  Currency { isoCode = "CAD", isoNumericCode=  124, decimalDigits= 2, symbol= "", name="Canadian Dollar"}
    933       ->  Currency { isoCode = "BYN", isoNumericCode=  933, decimalDigits= 2, symbol= "", name="Belarussian Ruble"}
    756       ->  Currency { isoCode = "CHF", isoNumericCode=  756, decimalDigits= 2, symbol= "", name="Swiss Franc"}
    otherwise ->  Currency { isoCode = "NA" , isoNumericCode=  0  , decimalDigits= 2, symbol= "", name="Uknown"}
