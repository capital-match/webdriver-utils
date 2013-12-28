{-# LANGUAGE OverloadedStrings #-}
-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and it is intended that you just import
-- @Test.Hspec.WebDriver@ and not @Test.Hspec@.  If you need to import @Test.Hspec@, you should do
-- so using a qualified import.
--
-- >{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- >module XKCD where
-- >
-- >import Data.Typeable (Typeable)
-- >import Test.Hspec.WebDriver
-- >import Test.WebDriver.Commands
-- >import qualified Test.WebDriver.Capabilities as W
-- >
-- >data TestCaps = Firefox
-- >              | Chrome
-- >    deriving (Show,Eq,Enum,Bounded,Typeable)
-- >
-- >withAll :: [TestCaps]
-- >withAll = [minBound..maxBound]
-- >
-- >instance TestCapabilities TestCaps where
-- >    matchesCaps Firefox (W.Capabilities { W.browser = W.Firefox _ _ _}) = True
-- >    matchesCaps Chrome (W.Capabilities { W.browser = W.Chrome _ _ _ _}) = True
-- >    matchesCaps _ _ = False
-- >
-- >    newCaps Firefox = return W.defaultCaps -- default caps use firefox
-- >    newCaps Chrome = return W.defaultCaps { W.browser = W.chrome }
-- >
-- >main :: IO ()
-- >main = hspec $
-- >    describe "XKCD Tests" $ do
-- >        it "checks hover text of 327" (with Firefox) $ do
-- >            openPage "http://www.xkcd.com/327/"
-- >            e <- findElem $ ByCSS "div#comic > img"
-- >            e `shouldBeTag` "img"
-- >            e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        it "checks title of 303" withAll $ do
-- >            openPage "http://www.xkcd.com/303/"
-- >            e <- findElem $ ById "ctitle"
-- >            e `shouldBeTag` "div"
-- >            e `shouldHaveText` "Compiling"
--
-- The above code assumes selenium-server-standalone is running on @127.0.0.1:4444@ at path
-- @\/wd\/hub@ (this is the default).  You can configure this using `createSessionManager'`. 
module Test.Hspec.WebDriver(
  -- * Webdriver
    TestCapabilities(..)
  , it
  , with
  , createSessionManager
  , createSessionManager'

  -- * Expectations
  , shouldBe
  , shouldBeTag
  , shouldHaveText
  , shouldHaveAttr
  , shouldReturn
  , shouldThrow

  -- * Re-exports from hspec
  , hspec
  , Spec
  , describe
  , context
  , pending
  , pendingWith
  , before
  , after
  , around
  , parallel
) where

import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (try, Exception)
import Test.Hspec.Core (Result(..), fromSpecList, SpecTree(..), Item(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, it)
import Test.WebDriver
import qualified Test.Hspec as H
import qualified Data.Text as T

import Test.Hspec.WebDriver.Internal

-- | Create a test for a list of capabilities.  The test will be executed once for each capability
-- in the list.  By default, the test on all the capabilities will be executed serially.  You can
-- use 'parallel' to execute the test on all capabilities in parallel.
--
-- To run the test, a webdriver session is allocated from a pool of sessions (in a thread-safe
-- manner).  The pools will be initialized automatically from existing sessions the first time a
-- test is run; you can explicitly create the pools using 'createSessionManager'.
it :: (Show c, TestCapabilities c) => String -> [c] -> WD () -> Spec
it msg [] _ = H.it msg $ pending
it msg cs test = describe msg $ fromSpecList $ map mkSpec cs
    where
        mkSpec c = SpecItem $ Item
                    { itemIsParallelizable = False
                    , itemRequirement = "with " ++ show c
                    , itemExample = \_ action -> do
                        action (runWD defaultSession $ withCaps c test)
                        return Success
                    }

-- | Create a list of one element.  Intended to be used to run a test on a single capability using
-- 'it'.
with :: a -> [a]
with a = [a]

-- | 'H.shouldBe' lifted into the 'WD' monad.
shouldBe :: (Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

-- | Asserts that the given element matches the given tag.
shouldBeTag :: Element -> T.Text -> WD ()
e `shouldBeTag` name = do
    t <- tagName e
    liftIO $ assertEqual ("tag of " ++ show e) name t

-- | Asserts that the given element has the given text.
shouldHaveText :: Element -> T.Text -> WD ()
e `shouldHaveText` txt = do
    t <- getText e
    liftIO $ assertEqual ("text of " ++ show e) txt t

-- | Asserts that the given elemnt has the attribute given by @(attr name, value)@.
shouldHaveAttr :: Element -> (T.Text, T.Text) -> WD ()
e `shouldHaveAttr` (a, txt) = do
    t <- attr e a
    liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e) (Just txt) t

-- | Asserts that the action returns the expected result.
shouldReturn :: (Show a, Eq a) => WD a -> a -> WD ()
action `shouldReturn` expected = action >>= (\a -> liftIO $ a `H.shouldBe` expected)

-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e) => WD a -> e -> WD ()
shouldThrow w expected = do
    r <- try w
    case r of
        Left err -> err `shouldBe` expected
        Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected
