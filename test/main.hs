{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Test.Hspec (hspec)

import Specs

startServer :: IO ThreadId
startServer = forkIO $
    run 3456 $ staticApp $ defaultFileServerSettings "test/www"

{-stop :: ThreadId -> IO ()
stop t = do
    runWD defaultSession $ do
        ss <- sessions
        mapM_ (\(x,_) -> putSession defaultSession {wdSessId = Just x} >> closeSession) ss

    killThread t-}

main :: IO ()
main = bracket startServer killThread $ \_ ->
    hspec specs
