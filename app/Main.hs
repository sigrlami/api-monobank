{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           System.Environment

import           Api

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    let delay = 30000
    a       <- getArgs
    timeVar <- newTVarIO (delay*1000)
    case a of
      ["-d"]    -> return ()
      otherwise -> return ()
