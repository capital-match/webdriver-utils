{-# LANGUAGE OverloadedStrings #-}
module NgSpec where

import Test.Hspec.WebDriver
import Test.WebDriver.Commands.Angular

ngSpec :: Spec
ngSpec = session "Angular webdriver commands" $ using [chromeCaps] $ do
    it "opens the page" $ runWD $ do
        openPage "http://localhost:3456/index.html"
        waitForAngular "body" `shouldReturn` True

    it "finds elements by binding" $ runWD $ do
        binding <- findNg $ ByBinding "{{a}}"
        binding `shouldBeTag` "h1"
        binding `shouldHaveText` "Hello A!"

    it "does not find missing model elements" $ runWD $ do
        findNgs (ByModel "qqq") `shouldReturn` []
        findNg (ByModel "qqq") `shouldThrow` NgException "Selector ByModel \"qqq\" returned []"

    it "finds input elements" $ runWD $ do
        i <- findNg $ ByModel "xxx"
        i `shouldBeTag` "input"
        sendKeys "John" i

        xHead <- findNg $ ByBinding "{{xxx}}"
        xHead `shouldBeTag` "h1"
        xHead `shouldHaveText` "X John"

    it "finds textarea elements" $ runWD $ do
        t <- findNg $ ByModel "yyy"
        t `shouldBeTag` "textarea"
        sendKeys "Mark" t

        yHead <- findNg $ ByBinding "{{yyy}}"
        yHead `shouldBeTag` "h2"
        yHead `shouldHaveText` "Y Mark"

    it "finds select elements" $ runWD $ do
        s <- findNg $ ByModel "zzz"
        s `shouldBeTag` "select"
        opt <- findElemFrom s $ ByCSS "option[value=\"Bar\"]"
        click opt

        zHead <- findNg $ ByBinding "{{zzz}}"
        zHead `shouldBeTag` "h3"
        zHead `shouldHaveText` "Z Bar"

    it "finds selected option inside select opt" $ runWD $ do
        opt <- findNg $ BySelectedOption "zzz"
        opt `shouldBeTag` "option"
        opt `shouldHaveText` "Bar"

    it "finds model elements of different types" $ runWD $ do
        [i, t, s] <- findNgs $ ByModel "name"
        i `shouldBeTag` "input"
        t `shouldBeTag` "textarea"
        s `shouldBeTag` "select"

    describe "ng-repeat" $ do

        it "finds all repeater rows" $ runWD $ do
            [r1, r2, r3] <- findRepeaters $ ByRows "dog in dogs1"
            mapM_ (`shouldBeTag`"li") [r1, r2, r3]
            r1 `shouldHaveText` "Spot mutt"
            r2 `shouldHaveText` "Spike poodle"
            r3 `shouldHaveText` "Jupiter bulldog"

            findRepeaters (ByRows "cat in cats") `shouldReturn` []
            findRepeater (ByRows "cat in cats") `shouldThrow` NgException "Selector ByRows \"cat in cats\" returned []"

        it "finds a single repeater row" $ runWD $ do
            r2 <- findRepeater $ ByRow "dog in dogs1" 1
            r2 `shouldBeTag` "li"
            r2 `shouldHaveText` "Spike poodle"

            findRepeaters (ByRow "cat in cats" 1) `shouldReturn` []
            findRepeater (ByRow "cat in cats" 1) `shouldThrow` NgException "Selector ByRow \"cat in cats\" 1 returned []"

        it "finds a repeater column" $ runWD $ do
            [c1, c2, c3] <- findRepeaters $ ByColumn "dog in dogs1" "{{dog.name}}"
            mapM_ (`shouldBeTag`"span") [c1, c2, c3]

            c1 `shouldHaveText` "Spot"
            c2 `shouldHaveText` "Spike"
            c3 `shouldHaveText` "Jupiter"

            findRepeaters (ByColumn "cat in cats" "{{cat.name}}") `shouldReturn` []
            findRepeater (ByColumn "cat in cats" "{{cat.name}}") `shouldThrow`
                NgException "Selector ByColumn \"cat in cats\" \"{{cat.name}}\" returned []"

        it "finds a repeater by row and column" $ runWD $ do
            c2 <- findRepeater $ ByRowAndCol "dog in dogs1" 1 "{{dog.breed}}"
            c2 `shouldBeTag` "span"
            c2 `shouldHaveText` "poodle"

            findRepeaters (ByRowAndCol "cat in cats" 12 "{{cat.name}}") `shouldReturn` []
            findRepeater (ByRowAndCol "cat in cats" 22 "{{cat.name}}") `shouldThrow`
                NgException "Selector ByRowAndCol \"cat in cats\" 22 \"{{cat.name}}\" returned []"

    describe "ng-repeat-start and ng-repeat-end" $ do

        it "finds all repeater rows" $ runWD $ do
            [r1, r1', r2, r2', r3, r3'] <- findRepeaters $ ByRows "dog in dogs2"
            mapM_ (`shouldBeTag`"li") [r1, r2, r3, r1', r2', r3']
            r1  `shouldHaveText` "Spot"
            r1' `shouldHaveText` "mutt"
            r2  `shouldHaveText` "Spike"
            r2' `shouldHaveText` "poodle"
            r3  `shouldHaveText` "Jupiter"
            r3' `shouldHaveText` "bulldog"

        it "finds a single repeater row" $ runWD $ do
            r2 <- findRepeaters $ ByRow "dog in dogs2" 1
            length r2 `shouldBe` 2
            mapM_ (`shouldBeTag` "li") r2
            head r2 `shouldHaveText` "Spike"
            (r2 !! 1) `shouldHaveText` "poodle"

        it "finds a repeater column" $ runWD $ do
            [c1, c2, c3] <- findRepeaters $ ByColumn "dog in dogs2" "{{dog.name}}"
            mapM_ (`shouldBeTag`"span") [c1, c2, c3]

            c1 `shouldHaveText` "Spot"
            c2 `shouldHaveText` "Spike"
            c3 `shouldHaveText` "Jupiter"

        it "finds a repeater by row and column" $ runWD $ do
            c2 <- findRepeater $ ByRowAndCol "dog in dogs2" 1 "{{dog.breed}}"
            c2 `shouldBeTag` "span"
            c2 `shouldHaveText` "poodle"

    it "evaluates an angular expression" $ runWD $ do
        e <- findNg $ ByBinding "{{a}}"
        ngEvaluate e "cost | number:2" `shouldReturn` ("12.60" :: String)

    it "loads the location url" $ runWD $ do
        getLocationAbsUrl "body" `shouldReturn` "http://localhost:3456/index.html"

    it "loads {{cost}} from document" $ runWD $ do
        [s1, s2] <- findNgs $ ByBinding "{{cost}}"
        s1 `shouldBeTag` "span"
        s2 `shouldBeTag` "span"
        s1 `shouldHaveAttr` ("id", "span-one")
        s2 `shouldHaveAttr` ("id", "span-two")

    it "loads {{cost}} only from one element" $ runWD $ do
        d <- findElem $ ById "one"
        d `shouldBeTag` "div"
        s1 <- findNgFrom d $ ByBinding "{{cost}}"
        s1 `shouldBeTag` "span"
        s1 `shouldHaveAttr` ("id", "span-one")

    it "sets the location" $ runWD $ do
        setNgLocation "body" "foo"
        getLocationAbsUrl "body" `shouldReturn` "http://localhost:3456/index.html#/foo"
