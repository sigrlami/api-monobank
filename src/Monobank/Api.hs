{-# LANGUAGE OverloadedStrings #-}

module Monobank.Api
    ( getCurrencies
    , getPersonalInfo
    , getPersonalStatement
    , getPersonalStatementFull
    ) where

import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Network.HTTP.Conduit       (simpleHttp)

import           Monobank.Types
import           Monobank.Utils

--------------------------------------------------------------------------------

endpoint :: T.Text
endpoint = "https://api.monobank.ua"

-- Get a basic list of monobank currency rates.
-- Information is cached and updated no more than once every 5 minutes.
--
-- GET /bank/currency
getCurrencies :: IO [CurrencyPair]
getCurrencies = do
  let path = T.concat [endpoint, "/bank/currency"]
  -- results <- fmap decodeEither $ simpleHttp (T.unpack path)
  resp <- simpleHttp (T.unpack path)
  --putStrLn $ show $ resp
  let res = eitherDecode resp
  case res of
    Left err  -> do
      putStrLn err
      return $ []
    Right val -> do
      return $ val

-- GET /personal/client-info
getPersonalInfo = undefined


-- Receive an extract for the time from {to} to {to} time in seconds Unix time format.
-- The maximum time for which it is possible to extract an extract is 31 days
-- (2678400 seconds) Limit on the use of the function no more than 1 time in 60 seconds.
--
-- GET /personal/statement/{account}/{from}/{to}
getPersonalStatement = undefined


-- Get Personal Statement from beginning
-- 12 months will take 12 minutes
getPersonalStatementFull = undefined
