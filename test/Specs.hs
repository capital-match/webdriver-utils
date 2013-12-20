{-# LANGUAGE OverloadedStrings #-}
module Specs where

import Test.WebDriver.Commands
import Test.WebDriver.Commands.Angular

import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (try, Exception)
import Test.Hspec.Core (Example(..), Result(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow)
import qualified Test.Hspec as H
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Classes as W
import qualified Data.Text as T

data MySession = Firefox
            -- | Chrome

sessionCaps :: MySession -> W.Capabilities
sessionCaps Firefox = W.defaultCaps

matchingCaps :: MySession -> W.Capabilities -> Bool
matchingCaps Firefox (W.Capabilities { W.browser = W.Firefox _ _ _ }) = True
matchingCaps _ _ = False

data WithSession = WithSession MySession (W.WD ())
instance Example WithSession where
    evaluateExample (WithSession stype w) _ action = action (W.runWD session w') >> return Success
        where
            session = W.defaultSession
            w' = do ss <- sessions
                    case filter (\(_, caps) -> matchingCaps stype caps) ss of
                        ((s, _):_) -> W.putSession $ session { W.wdSessId = Just s }
                        _ -> do s <- createSession $ sessionCaps stype
                                W.putSession s
                    w
                        
shouldBe :: (Show a, Eq a) => a -> a -> W.WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

shouldBeTag :: Element -> T.Text -> W.WD ()
e `shouldBeTag` name = do
    t <- tagName e
    liftIO $ assertEqual ("tag of " ++ show e) name t

shouldHaveText :: Element -> T.Text -> W.WD ()
e `shouldHaveText` txt = do
    t <- getText e
    liftIO $ assertEqual ("text of " ++ show e) txt t

shouldHaveAttr :: Element -> (T.Text, T.Text) -> W.WD ()
e `shouldHaveAttr` (a, txt) = do
    t <- attr e a
    liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e) (Just txt) t

shouldReturn :: (Show a, Eq a) => W.WD a -> a -> W.WD ()
action `shouldReturn` expected = action >>= (\a -> liftIO $ a `H.shouldBe` expected)

shouldThrow :: (Show e, Eq e, Exception e) => W.WD a -> e -> W.WD ()
shouldThrow w expected = do
    r <- try w
    case r of
        Left err -> err `shouldBe` expected
        Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected

specs :: Spec
specs = describe "Angular webdriver commands" $ do
    it "finds elements by binding" $ WithSession Firefox $ do
        openPage "http://localhost:3456/index.html"
        _ <- waitForAngular "body" --`shouldReturn` True

        binding <- findNg $ ByBinding "{{a}}"
        binding `shouldBeTag` "h1"
        binding `shouldHaveText` "Hello A!"

    it "does not find missing model elements" $ WithSession Firefox $ do
        findNgs (ByModel "qqq") `shouldReturn` []
        findNg (ByModel "qqq") `shouldThrow` NgException "Selector ByModel \"qqq\" returned []"

    it "finds input elements" $ WithSession Firefox $ do
        i <- findNg $ ByModel "xxx"
        i `shouldBeTag` "input"
        sendKeys "John" i

        xHead <- findNg $ ByBinding "{{xxx}}"
        xHead `shouldBeTag` "h1"
        xHead `shouldHaveText` "X John"

    it "finds textarea elements" $ WithSession Firefox $ do
        t <- findNg $ ByModel "yyy"
        t `shouldBeTag` "textarea"
        sendKeys "Mark" t

        yHead <- findNg $ ByBinding "{{yyy}}"
        yHead `shouldBeTag` "h2"
        yHead `shouldHaveText` "Y Mark"

    it "finds select elements" $ WithSession Firefox $ do
        s <- findNg $ ByModel "zzz"
        s `shouldBeTag` "select"
        opt <- findElemFrom s $ ByCSS "option[value=\"Bar\"]"
        click opt

        zHead <- findNg $ ByBinding "{{zzz}}"
        zHead `shouldBeTag` "h3"
        zHead `shouldHaveText` "Z Bar"

    it "finds selected option inside select opt" $ WithSession Firefox $ do
        opt <- findNg $ BySelectedOption "zzz"
        opt `shouldBeTag` "option"
        opt `shouldHaveText` "Bar"

    it "finds model elements of different types" $ WithSession Firefox $ do
        [i, t, s] <- findNgs $ ByModel "name"
        i `shouldBeTag` "input"
        t `shouldBeTag` "textarea"
        s `shouldBeTag` "select"

    it "finds all repeater rows" $ WithSession Firefox $ do
        [r1, r2, r3] <- findRepeaters $ ByRows "dog in dogs"
        mapM_ (`shouldBeTag`"li") [r1, r2, r3]
        r1 `shouldHaveText` "Spot mutt"
        r2 `shouldHaveText` "Spike poodle"
        r3 `shouldHaveText` "Jupiter bulldog"

        findRepeaters (ByRows "cat in cats") `shouldReturn` []
        findRepeater (ByRows "cat in cats") `shouldThrow` NgException "Selector ByRows \"cat in cats\" returned []"

    it "finds a single repeater row" $ WithSession Firefox $ do
        r2 <- findRepeater $ ByRow "dog in dogs" 1
        r2 `shouldBeTag` "li"
        r2 `shouldHaveText` "Spike poodle"

        findRepeaters (ByRow "cat in cats" 1) `shouldReturn` []
        findRepeater (ByRow "cat in cats" 1) `shouldThrow` NgException "Selector ByRow \"cat in cats\" 1 returned []"

    it "finds a repeater column" $ WithSession Firefox $ do
        [c1, c2, c3] <- findRepeaters $ ByColumn "dog in dogs" "{{dog.name}}"
        mapM_ (`shouldBeTag`"span") [c1, c2, c3]

        c1 `shouldHaveText` "Spot"
        c2 `shouldHaveText` "Spike"
        c3 `shouldHaveText` "Jupiter"

        findRepeaters (ByColumn "cat in cats" "{{cat.name}}") `shouldReturn` []
        findRepeater (ByColumn "cat in cats" "{{cat.name}}") `shouldThrow`
            NgException "Selector ByColumn \"cat in cats\" \"{{cat.name}}\" returned []"

    it "finds a repeater by row and column" $ WithSession Firefox $ do
        c2 <- findRepeater $ ByRowAndCol "dog in dogs" 1 "{{dog.breed}}"
        c2 `shouldBeTag` "span"
        c2 `shouldHaveText` "poodle"

        -- These currently cause Javascript errors
        --findRepeaters (ByRowAndCol "cat in cats" 12 "{{cat.name}}") `shouldReturn` []
        --findRepeater (ByRowAndCol "cat in cats" 22 "{{cat.name}}") `shouldThrow`
        --    NgException "Selector ByRowAndCol \"cat in cats\" 22 \"{{cat.name}}\" returned []"

    it "evaluates an angular expression" $ WithSession Firefox $ do
        e <- findNg $ ByBinding "{{a}}"
        ngEvaluate e "cost | number:2" `shouldReturn` ("12.60" :: T.Text)

    it "loads the location url" $ WithSession Firefox $ do
        getLocationAbsUrl "body" `shouldReturn` "http://localhost:3456/index.html"

    it "loads {{cost}} from document" $ WithSession Firefox $ do
        [s1, s2] <- findNgs $ ByBinding "{{cost}}"
        s1 `shouldBeTag` "span"
        s2 `shouldBeTag` "span"
        s1 `shouldHaveAttr` ("id", "span-one")
        s2 `shouldHaveAttr` ("id", "span-two")

    it "loads {{cost}} only from one element" $ WithSession Firefox $ do
        d <- findElem $ ById "one"
        d `shouldBeTag` "div"
        s1 <- findNgFrom d $ ByBinding "{{cost}}"
        s1 `shouldBeTag` "span"
        s1 `shouldHaveAttr` ("id", "span-one")
