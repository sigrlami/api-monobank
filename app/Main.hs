{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Servant.Elm
import           System.Environment

import qualified Monobank.Api                as MBApi
import qualified Monobank.Types              as MBApi

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    let delay = 60000  -- 1 request per minute available
    a       <- getArgs
    timeVar <- newTVarIO (delay*1000)
    case a of
      ["-ge"]    -> do
        -- generate ELM API
        specsToDir [spec] "assets/generated/api/elm"
        return ()
      otherwise -> do
        putStrLn $ "Monobank | Not tokenized API access available for currencies only"
        putStrLn $ "Monobank | Getting last currency prices\n"
        mcurrs <- MBApi.getCurrencies
        --putStrLn $ show $ mcurrs
        mapM_ MBApi.showCurrencyPair mcurrs
        return ()


spec :: Spec
spec =
  Spec ["MonobankApi"]
       (defElmImports : generateElmForAPI (Proxy :: Proxy MBApi.MonobankAPI))
