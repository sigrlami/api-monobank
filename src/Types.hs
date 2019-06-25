module Types where


type Token = Text

data CurrencyPair where
  { cpCurrencyCodeA :: Int     -- ^ currency code from international identificator
  , cpCurrencyCodeB :: Int     -- ^ currency code from international identificator
  , cpDate          :: UTCTime -- ^
  , cpRateSell      :: Float   -- ^
  , cpRateBuy       :: Float   -- ^
  , cpRateCross     :: Float   -- ^
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)
