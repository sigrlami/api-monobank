module Api
    ( getCurrencies
    , getPersonalInfo
    , getPersonalStatement
    , getPersonalStatementFull
    , showCurrencyPair
    ) where

import           Data.Text as T

import           Types
import           Utils

--------------------------------------------------------------------------------

-- Get a basic list of monobank currency rates.
-- Information is cached and updated no more than once every 5 minutes.
--
-- GET /bank/currency
getCurrencies :: IO [CurrencyPair]
getCurrencies = do
  putStrLn "getting currencies"


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
