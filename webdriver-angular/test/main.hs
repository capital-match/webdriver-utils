{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Test.Hspec (hspec)

import NgSpecs

startServer :: IO ThreadId
startServer = forkIO $
    run 3456 $ staticApp $ defaultFileServerSettings "test/www"

main :: IO ()
main = bracket startServer killThread $ \_ ->
    hspec $ ngSpec
