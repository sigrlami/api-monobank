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
    , getPersonalInfoSigned'
    , getPersonalStatement'
    , getPersonalStatementSigned'
    , getPersonalStatementFull
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
    :> Header  "X-Token" Token
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
-- Corporate API description https://api.monobank.ua/docs/corporate.html

type MonobankCorporateAPI =

  -- POST /personal/auth/request
       "personal"
    :> "auth"
    :> "request"
    :> Header  "X-Key-Id"      T.Text
    :> Header  "X-Time"        T.Text
    :> Header  "X-Permissions" T.Text
    :> Header  "X-Sign"        T.Text
    :> Header  "X-Callback"    T.Text
    :> Post '[JSON] (Either String TokenRequest)

  -- Get /personal/auth/request
  :<|> "personal"
    :> "auth"
    :> "request"
    :> Header  "X-Key-Id"      T.Text
    :> Header  "X-Time"        T.Text
    :> Header  "X-Permissions" T.Text
    :> Header  "X-Sign"        T.Text
    :> Get '[JSON] (Either String TokenRequest)

  -- GET /personal/client-info
  :<|> "personal"
    :> "client-info"
    :> Header  "X-Key-Id"      T.Text
    :> Header  "X-Time"        T.Text
    :> Header  "X-Permissions" T.Text
    :> Header  "X-Sign"        T.Text
    :> Get '[JSON] User

  -- GET /personal/statement/{account}/{from}/{to}
  :<|> "personal"
    :> "statement"
    :> Header  "X-Key-Id"      T.Text
    :> Header  "X-Time"        T.Text
    :> Header  "X-Permissions" T.Text
    :> Header  "X-Sign"        T.Text
    :> Capture "account"       T.Text
    :> Capture "from"          T.Text
    :> Capture "to"            T.Text
    :> Get '[JSON] Statement

monobankCorporateAPI :: Proxy MonobankCorporateAPI
monobankCorporateAPI = Proxy

-- Derive call functions for the api
initializeAuth             :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> ClientM (Either String TokenRequest)
requestAuth                :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> ClientM (Either String TokenRequest)
getPersonalInfoSigned      :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> ClientM User
getPersonalStatementSigned :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> T.Text -> T.Text -> T.Text -> ClientM Statement
(     initializeAuth
 :<|> requestAuth
 :<|> getPersonalInfoSigned
 :<|> getPersonalStatementSigned ) = client monobankCorporateAPI

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
getPersonalInfo' :: Maybe Token                        -- ^ unique user token
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
getPersonalStatement' :: Maybe Token
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

getPersonalStatement'' :: Maybe Token
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

getPersonalInfoSigned' = undefined

getPersonalStatementSigned' = undefined

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
-- 12 months will take 12 minutes,
getPersonalStatementFull :: Maybe Token
                         -> T.Text
                         -> T.Text
                         -> T.Text
                         -> IO (Either String Statement)
getPersonalStatementFull tknM acc from to = do
  let estimatedDownloadTime = 12 -- due to API restrictions, we can make only 1 call per minute
  putStrLn $ "monobank-api: due to API restrictions, we can make only 1 call per minute"
  putStrLn $ "              operation will some time, approx ~ " ++ (show estimatedDownloadTime) ++ " min"

  -- TODO: do actual calls respetively to API restrictions

  return $ Left "Error"
