{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import           Data.Char
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Elm
import           Servant.Elm
import           System.Environment

import qualified Monobank.Api                as MBApi
import qualified Monobank.Types              as MBApi
import qualified Monobank.Utils              as MBApi

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
       (  defElmImports
         : toElmTypeSource (Proxy :: Proxy MBApi.User)
         : toElmTypeSource (Proxy :: Proxy MBApi.Currency)
         : toElmTypeSource (Proxy :: Proxy MBApi.Account)
         : toElmTypeSource (Proxy :: Proxy MBApi.Statement)
         : toElmTypeSource (Proxy :: Proxy MBApi.CurrencyPair)
         : toElmDecoderSource (Proxy :: Proxy MBApi.Currency)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "u"})  (Proxy :: Proxy MBApi.User)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "ac"}) (Proxy :: Proxy MBApi.Account)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "st"}) (Proxy :: Proxy MBApi.Statement)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "cp"}) (Proxy :: Proxy MBApi.CurrencyPair)
         : generateElmForAPI (Proxy :: Proxy MBApi.MonobankAPI))


initCap :: T.Text -> T.Text
initCap t =
  case T.uncons t of
    Nothing      -> t
    Just (c, cs) -> T.cons (Data.Char.toUpper c) cs

withPrefix :: T.Text -> T.Text -> T.Text
withPrefix prefix s = prefix <> initCap s

withoutPrefix :: T.Text -> T.Text -> T.Text
withoutPrefix prefix s =
  MBApi.uncapFst' $ fromMaybe s $ T.stripPrefix prefix s
