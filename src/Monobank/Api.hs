{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Monobank.Api
    ( getCurrencies
    , getCurrencyRates'
    , getPersonalInfo'
    , getPersonalStatement'
    , getPersonalStatementFull
    , MonobankAPI
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.HTTP.Client        (Manager, newManager)
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Conduit       (simpleHttp)
import qualified Network.HTTP.Simple        as S
import           Servant.API
import           Servant.Client

import           Monobank.Types
import           Monobank.Utils

--------------------------------------------------------------------------------

endpoint = "api.monobank.ua"

type MonobankAPI =
       "bank"
    :> "currency"
    :> Get '[JSON] [CurrencyPair]

   -- GET /personal/client-info
  :<|> "personal"
    :> "client-info"
    :> Header "X-Token" T.Text
    :> Get '[JSON] User

  -- GET /personal/statement/{account}/{from}/{to}
  :<|> "personal"
    :> "statement"
    :> Header  "X-Token" T.Text
    :> Capture "account" T.Text
    :> Capture "from"    T.Text
    :> Capture "to"      T.Text
    :> Get '[JSON] Statement

monobankAPI :: Proxy MonobankAPI
monobankAPI = Proxy

-- Derive call functions for the api
getCurrencyRates     :: ClientM [CurrencyPair]
getPersonalInfo      :: Maybe T.Text -> ClientM User
getPersonalStatement :: Maybe T.Text -> T.Text -> T.Text -> T.Text -> ClientM Statement
(     getCurrencyRates
 :<|> getPersonalInfo
 :<|> getPersonalStatement ) = client monobankAPI

--------------------------------------------------------------------------------

-- | Get a basic list of monobank currency rates
-- |
getCurrencyRates' :: IO (Either ServantError [CurrencyPair])
getCurrencyRates' = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM getCurrencyRates env
  where
    host = (BaseUrl Https endpoint 443 "")

-- | Ger bank user personal information
-- |
getPersonalInfo' :: Maybe T.Text                        -- ^ unique user token
                 -> IO (Either ServantError User)
getPersonalInfo' tkn = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getPersonalInfo tkn) env
  where
    host = (BaseUrl Https endpoint 443 "")


-- | Receive an extract for the time from {to} to {to} time in seconds Unix time format.
-- | The maximum time for which it is possible to extract an extract is 31 days
-- | (2678400 seconds) Limit on the use of the function no more than 1 time in 60 seconds.
--
getPersonalStatement' :: Maybe T.Text
                      -> T.Text
                      -> T.Text
                      -> T.Text
                      -> IO (Either ServantError Statement)
getPersonalStatement' tkn accM fromM toM = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getPersonalStatement tkn accM fromM toM) env
  where
    host = (BaseUrl Https endpoint 443 "")

getPersonalStatement'' :: Maybe T.Text
                      -> T.Text
                      -> T.Text
                      -> T.Text
                      -> IO (Either ServantError Statement)
getPersonalStatement'' tknM acc from to = do
  mgr <- newManager tlsManagerSettings
  let env   = ClientEnv mgr host Nothing
      from' = T.init . T.pack . show . utcTimeToPOSIXSeconds $ (read (T.unpack from) :: UTCTime)
      to'   = T.init . T.pack . show . utcTimeToPOSIXSeconds $ (read (T.unpack to)   :: UTCTime)
  putStrLn $ show from'
  runClientM (getPersonalStatement tknM acc from' to') env
  where
    host  = (BaseUrl Https endpoint 443 "")

--------------------------------------------------------------------------------

-- Get a basic list of monobank currency rates.
-- Information is cached and updated no more than once every 5 minutes.
--
-- GET /bank/currency
getCurrencies :: IO [CurrencyPair]
getCurrencies = do
  let path = T.concat [T.pack endpoint, "/bank/currency"]
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


-- Get Personal Statement from beginning
-- 12 months will take 12 minutes
getPersonalStatementFull = undefined
