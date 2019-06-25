module Api
    ( getCurrencies
    ) where


-- Get a basic list of monobank currency rates.
-- Information is cached and updated no more than once every 5 minutes.
-- GET /bank/currency
getCurrencies :: IO [CurrencyPair]
getCurrencies = do
  putStrLn "getting currencies"
