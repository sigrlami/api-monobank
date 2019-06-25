{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           System.Environment

import qualified Api                         as MBApi

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    let delay = 60000  -- 1 request per minute available
    a       <- getArgs
    timeVar <- newTVarIO (delay*1000)
    case a of
      ["-t"]    -> do
        -- read token from file

        return ()
      otherwise -> do
        putStrLn $ "monobank | Not tokenized API access available for currencies only"
        putStrLn $ "monobank | Getting last currency prices"
        mcurr <- MBApi.getCurrencies
        putStrLn $ show $ mcurr
        return ()
