{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module ManagerSpecs where

import Control.Concurrent (threadDelay)
import Test.Hspec.WebDriver
import Test.WebDriver.Commands.Angular

mSpecs :: Spec
mSpecs = describe "session manager tests" $ do
    parallel $ it "runs in parallel" $ using [Firefox, Chrome] $ do
        openPage "http://localhost:3456/index.html"
        waitForAngular "body" `shouldReturn` True

        i <- findNg $ ByModel "xxx"
        i `shouldBeTag` "input"
        sendKeys "John" i

        liftIO $ threadDelay $ 25 * 10^(6::Int)

        sendKeys " Mark" i
        xHead <- findNg $ ByBinding "{{xxx}}"
        xHead `shouldBeTag` "h1"
        xHead `shouldHaveText` "X John Mark"

    parallel $ do
        describe "Firefox 1" $
            it "waits for a while" $ using Firefox $ do
                openPage "http://localhost:3456/index.html"
                waitForAngular "body" `shouldReturn` True
                
                i <- findNg $ ByModel "xxx"
                sendKeys "Firefox 1" i
                
                liftIO $ threadDelay $ 2 * 10^(6::Int)

        describe "Firefox 2" $
            it "waits for a while" $ using Firefox $ do
                openPage "http://localhost:3456/index.html"
                waitForAngular "body" `shouldReturn` True
                
                i <- findNg $ ByModel "xxx"
                sendKeys "Firefox 2" i
                
                liftIO $ threadDelay $ 2 * 10^(6::Int)

        describe "Firefox 3" $
            it "waits for a while" $ using Firefox $ do
                openPage "http://localhost:3456/index.html"
                waitForAngular "body" `shouldReturn` True
                
                i <- findNg $ ByModel "xxx"
                sendKeys "Firefox 3" i
                
                liftIO $ threadDelay $ 2 * 10^(6::Int)

