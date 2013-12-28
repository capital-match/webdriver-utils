{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
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
-- >import qualified Test.WebDriver as W
-- >
-- >data TestCaps = Firefox
-- >              | Chrome
-- >    deriving (Show,Eq,Enum,Bounded,Typeable)
-- >
-- >usingAll :: W.WD () -> WdExpectation TestCaps
-- >usingAll = UsingCaps [minBound..maxBound]
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
-- >        it "checks hover text of 327" $ using Firefox $ do
-- >            openPage "http://www.xkcd.com/327/"
-- >            e <- findElem $ ByCSS "div#comic > img"
-- >            e `shouldBeTag` "img"
-- >            e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        it "checks title of 303" $ usingAll $ do
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
  , WdExpectation(..)
  , using
  , it
  , WdExample
  , HspecExample(..)
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
import Test.Hspec.Core (Result(..), fromSpecList, SpecTree(..), Item(..), Example(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, it)
import Test.WebDriver
import qualified Test.Hspec as H
import qualified Data.Text as T

import Test.Hspec.WebDriver.Internal

-- | An expectation to check against a list of capabilities.  The test will be executed once for each
-- capability in the list.  By default, the tests  will be executed serially.  You can use
-- 'parallel' to execute the test on all capabilities in parallel, by using something like
--
-- >parallel $ it "loads the home page" $ UsingCaps [Firefox, Chrome] $ do
-- >   openPage "http://localhost/"
-- >   ...
--
-- To run the test, a webdriver session is allocated from a pool of sessions (in a thread-safe
-- manner).  The pools will be initialized automatically from existing sessions the first time a
-- test is run; you can explicitly create the pools using 'createSessionManager'.
data WdExpectation cap = UsingCaps [cap] (WD ())

instance (Show c, TestCapabilities c) => WdExample (WdExpectation c) where
    evaluateWdExamples (UsingCaps cs test) = map mkItem cs
        where
            mkItem c = ("using " ++ show c, eval c)

            eval :: TestCapabilities c => c -> params -> (IO () -> IO ()) -> IO Result
            eval c _ action = do action (runWD defaultSession $ withCaps c test)
                                 return Success

-- | Create a 'WdExpectation' to pass to 'it' from a single set of capabilities instead of a list.
-- For example,
--
-- >it "opens XKCD 303" $ using Firefox $ do
-- >    openPage "http://www.xkcd.com/303/"
-- >    ...
--
-- To create an expectation which uses all capabilities, you should also define
--
-- >usingAll :: WD () -> WdExpectation TestCaps
-- >usingAll = UsingCaps [minBound..maxBound]
using :: TestCapabilities c => c -> WD () -> WdExpectation c
using c = UsingCaps [c]

-- | A newtype to turn any hspec 'Example' into a 'WdExample'.
newtype HspecExample a = Ex a

instance Example a => WdExample (HspecExample a) where
    evaluateWdExamples (Ex a) = [("", evaluateExample a)]

-- | Create a spec item from a 'WdExample', which is primarily intended to be a 'WdExpectation'.
--
-- Any hspec 'Example' (hunit, quickcheck property, etc) can also be used by using @Ex@ from the
-- 'HspecExample' newtype, but typically I write webdriver tests in one file which imports
-- @Test.Hspec.WebDriver@ and write non-webdriver tests in a different file which imports
-- "Test.Hspec" for the normal @it@.  The 'Spec' created from both files can then be combined into a
-- single 'Spec'.
it :: WdExample a => String -> a -> Spec
it msg a = spec
    where
        mkItem m f = SpecItem Item { itemIsParallelizable = False
                                   , itemRequirement = m
                                   , itemExample = f
                                   }
        spec = case evaluateWdExamples a of
                [] -> fromSpecList []
                [("",f)] -> fromSpecList [mkItem (msg) f]
                [(m,f)] -> fromSpecList [mkItem (msg ++ " " ++ m) f]
                ss -> describe msg $ fromSpecList $ map (uncurry mkItem) ss

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
